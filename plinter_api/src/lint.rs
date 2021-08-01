use crate::span::Symbol;

/// Specification of a single lint.
#[derive(Copy, Clone, Debug)]
pub struct Lint {
    /// A string identifier for the lint.
    ///
    /// This identifies the lint in attributes and in command-line arguments.
    /// In those contexts it is always lowercase, but this field is compared
    /// in a way which is case-insensitive for ASCII characters. This allows
    /// `declare_lint!()` invocations to follow the convention of upper-case
    /// statics without repeating the name.
    ///
    /// The name is written with underscores, e.g., "unused_imports".
    /// On the command line, underscores become dashes.
    ///
    /// See <https://rustc-dev-guide.rust-lang.org/diagnostics.html#lint-naming>
    /// for naming guidelines.
    pub name: &'static str,

    /// Default level for the lint.
    ///
    /// See <https://rustc-dev-guide.rust-lang.org/diagnostics.html#diagnostic-levels>
    /// for guidelines on choosing a default level.
    pub default_level: Level,

    /// Description of the lint or the issue it detects.
    ///
    /// e.g., "imports that are never used"
    pub desc: &'static str,

    /// Starting at the given edition, default to the given lint level. If this is
    /// `None`, then use `default_level`.
    pub edition_lint_opts: Option<(Edition, Level)>,

    /// `true` if this lint is reported even inside expansions of external macros.
    pub report_in_external_macro: bool,

    pub future_incompatible: Option<FutureIncompatibleInfo>,

    pub is_plugin: bool,

    /// `Some` if this lint is feature gated, otherwise `None`.
    pub feature_gate: Option<Symbol>,

    pub crate_level_only: bool,
}

/// Indicates the confidence in the correctness of a suggestion.
///
/// All suggestions are marked with an `Applicability`. Tools use the applicability of a
/// suggestion to determine whether it should be automatically applied or if the user
/// should be consulted before applying the suggestion.
#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub enum Applicability {
    /// The suggestion is definitely what the user intended, or maintains the exact
    /// meaning of the code. This suggestion should be automatically applied.
    ///
    /// In case of multiple `MachineApplicable` suggestions (whether as part of
    /// the same `multipart_suggestion` or not), all of them should be
    /// automatically applied.
    MachineApplicable,

    /// The suggestion may be what the user intended, but it is uncertain. The suggestion
    /// should result in valid Rust code if it is applied.
    MaybeIncorrect,

    /// The suggestion contains placeholders like `(...)` or `{ /* fields */ }`. The
    /// suggestion cannot be applied automatically because it will not result in
    /// valid Rust code. The user will need to fill in the placeholders.
    HasPlaceholders,

    /// The applicability of the suggestion is unknown.
    Unspecified,
}

/// Setting for how to handle a lint.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Level {
    Allow,
    Warn,
    Deny,
    Forbid,
}

/// Extra information for a future incompatibility lint.
#[derive(Copy, Clone, Debug)]
pub struct FutureIncompatibleInfo {
    /// e.g., a URL for an issue/PR/RFC or error code
    pub reference: &'static str,
    /// The reason for the lint used by diagnostics to provide
    /// the right help message
    pub reason: FutureIncompatibilityReason,
    /// Whether to explain the reason to the user.
    ///
    /// Set to false for lints that already include a more detailed
    /// explanation.
    pub explain_reason: bool,
}

/// The reason for future incompatibility
#[derive(Copy, Clone, Debug)]
pub enum FutureIncompatibilityReason {
    /// This will be an error in a future release
    /// for all editions
    FutureReleaseError,
    /// This will be an error in a future release, and
    /// Cargo should create a report even for dependencies
    FutureReleaseErrorReportNow,
    /// Previously accepted code that will become an
    /// error in the provided edition
    EditionError(Edition),
    /// Code that changes meaning in some way in
    /// the provided edition
    EditionSemanticsChange(Edition),
}

impl FutureIncompatibilityReason {
    pub fn edition(self) -> Option<Edition> {
        match self {
            Self::EditionError(e) => Some(e),
            Self::EditionSemanticsChange(e) => Some(e),
            _ => None,
        }
    }
}

impl FutureIncompatibleInfo {
    pub const fn default_fields_for_macro() -> Self {
        FutureIncompatibleInfo {
            reference: "",
            reason: FutureIncompatibilityReason::FutureReleaseError,
            explain_reason: true,
        }
    }
}

impl Lint {
    pub const fn new(name: &'static str, desc: &'static str) -> Self {
        Lint {
            name,
            default_level: Level::Warn,
            desc,
            edition_lint_opts: None,
            is_plugin: false,
            report_in_external_macro: false,
            future_incompatible: None,
            feature_gate: None,
            crate_level_only: false,
        }
    }

    pub const fn default_fields_for_macro() -> Self {
        Lint {
            name: "",
            default_level: Level::Forbid,
            desc: "",
            edition_lint_opts: None,
            is_plugin: false,
            report_in_external_macro: false,
            future_incompatible: None,
            feature_gate: None,
            crate_level_only: false,
        }
    }

    /// Gets the lint's name, with ASCII letters converted to lowercase.
    pub fn name_lower(&self) -> String { self.name.to_ascii_lowercase() }

    pub fn default_level(&self, edition: Edition) -> Level {
        self.edition_lint_opts
            .filter(|(e, _)| *e <= edition)
            .map(|(_, l)| l)
            .unwrap_or(self.default_level)
    }
}
/// The edition of the compiler. (See [RFC 2052](https://github.com/rust-lang/rfcs/blob/master/text/2052-epochs.md).)
#[derive(Clone, Copy, Hash, PartialEq, PartialOrd, Debug, Eq)]
#[non_exhaustive]
pub enum Edition {
    // When adding new editions, be sure to do the following:
    //
    // - update the `ALL_EDITIONS` const
    // - update the `EDITION_NAME_LIST` const
    // - add a `rust_####()` function to the session
    // - update the enum in Cargo's sources as well
    //
    // Editions *must* be kept in order, oldest to newest.
    /// The 2015 edition
    Edition2015,
    /// The 2018 edition
    Edition2018,
    /// The 2021 edition
    Edition2021,
}

/// Identifies a lint known to the compiler.
#[derive(Clone, Copy, Debug)]
pub struct LintId {
    // Identity is based on pointer equality of this field.
    pub lint: &'static Lint,
}

impl PartialEq for LintId {
    fn eq(&self, other: &LintId) -> bool { std::ptr::eq(self.lint, other.lint) }
}

impl Eq for LintId {}

impl std::hash::Hash for LintId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let ptr = self.lint as *const Lint;
        ptr.hash(state);
    }
}

impl LintId {
    /// Gets the `LintId` for a `Lint`.
    pub fn of(lint: &'static Lint) -> LintId { LintId { lint } }

    pub fn lint_name_raw(&self) -> &'static str { self.lint.name }

    /// Gets the name of the lint.
    pub fn to_string(&self) -> String { self.lint.name_lower() }
}

pub(crate) use crate::error::Error;
pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

pub(crate) use std::format as f;

/// Wrapper around a type `T` that can be used to implement external traits for `T`.
///
/// # Examples
///
/// ```
/// use std::convert::From;
///
/// struct Wrapper<T>(pub T);
///
/// impl<T> From<T> for Wrapper<T> {
///     fn from(t: T) -> Self {
///         Wrapper(t)
///     }
/// }
///
///
/// let value = Wrapper::from(42);
/// assert_eq!(value.0, 42);
/// ```
pub(crate) struct Wrapper<T>(pub T);

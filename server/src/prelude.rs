pub(crate) use crate::error::Error;
pub(crate) type Result<T> = core::result::Result<T, Error>;

pub(crate) use std::format as f;

pub(crate) struct W<T>(pub T);

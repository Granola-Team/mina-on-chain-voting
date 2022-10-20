use serde::de::DeserializeOwned;
use serde_json::{self, Deserializer};
use std::io::{self, Read};

pub fn read_skipping_ws(mut reader: impl Read) -> io::Result<u8> {
    loop {
        let mut byte = 0u8;
        reader.read_exact(std::slice::from_mut(&mut byte))?;
        if !byte.is_ascii_whitespace() {
            return Ok(byte);
        }
    }
}

#[cfg(test)]
#[test]
fn read_skipping_ws_reads_data() {
    let array_eights: [u8; 1] = [1];
    let result = read_skipping_ws(&array_eights[..]); 
    assert!(matches!(result, Result::Ok(_)));
    let result = result.unwrap();
    assert_eq!(result, 1);
}

#[cfg(test)]
#[test]
fn read_skipping_ws_skips_whitespace_all() {
    let array_eights: [u8; 5] = [0x20,0x09,0x0A,0x0C,0x0D];
    let result = read_skipping_ws(&array_eights[..]);
    assert!(matches!(result, Result::Err(_))); 
}

fn invalid_data(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg)
}

fn deserialize_single<T: DeserializeOwned, R: Read>(reader: R) -> io::Result<T> {
    let next_obj = Deserializer::from_reader(reader).into_iter::<T>().next();
    match next_obj {
        Some(result) => result.map_err(Into::into),
        None => Err(invalid_data("premature EOF")),
    }
}

#[cfg(test)]
#[test]
fn deserialize_single_deserializes_json() {
    use crate::models::DBResponse;
    let json = r#"{
        "account":  "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
        "memo":     "magenta",
        "height":   10,
        "status":   "Canonical",
        "timestamp": 20901343
    }"#.as_bytes();
    
    let deserialized: DBResponse = deserialize_single(json).unwrap();
    let db_response = DBResponse {
        account: String::from("bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu"),
        memo: String::from("magenta"),
        height: 10,
        status: crate::models::BlockStatus::Canonical,
        timestamp: 20901343,
    };

    assert_eq!(deserialized, db_response);
}

#[cfg(test)]
#[test]
fn deserialize_single_detects_early_eof() {
    use crate::models::DBResponse;
    let json = r#""#.as_bytes();
    
    if let Err(e) = deserialize_single::<DBResponse, _>(json) {
        assert_eq!(&e.to_string(), "premature EOF")
    } else {
        assert!(false)
    }
}

fn yield_next_obj<T: DeserializeOwned, R: Read>(
    mut reader: R,
    at_start: &mut bool,
) -> io::Result<Option<T>> {
    if !*at_start {
        *at_start = true;
        if read_skipping_ws(&mut reader)? == b'[' {
            // read the next char to see if the array is empty
            let peek = read_skipping_ws(&mut reader)?;
            if peek == b']' {
                Ok(None)
            } else {
                deserialize_single(io::Cursor::new([peek]).chain(reader)).map(Some)
            }
        } else {
            Err(invalid_data("`[` not found"))
        }
    } else {
        match read_skipping_ws(&mut reader)? {
            b',' => deserialize_single(reader).map(Some),
            b']' => Ok(None),
            _ => Err(invalid_data("`,` or `]` not found")),
        }
    }
}

#[cfg(test)]
#[test]
fn yeild_next_obj_yeilds_json() {
    use crate::models::DBResponse;
    let json = r#"[
        {
            "account":  "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            "memo":     "magenta",
            "height":   10,
            "status":   "Canonical",
            "timestamp": 20901343
        }
    ]"#.as_bytes();
    
    let obj_yielded: DBResponse = yield_next_obj(json, &mut false).unwrap().unwrap();
    let db_response = DBResponse {
        account: String::from("bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu"),
        memo: String::from("magenta"),
        height: 10,
        status: crate::models::BlockStatus::Canonical,
        timestamp: 20901343,
    };

    assert_eq!(obj_yielded, db_response);
}

pub fn iter<T: DeserializeOwned, R: Read>(
    mut reader: R,
) -> impl Iterator<Item = (usize, Result<T, io::Error>)> {
    let mut at_start = false;
    std::iter::from_fn(move || yield_next_obj(&mut reader, &mut at_start).transpose()).enumerate()
}

#[cfg(test)]
#[test]
fn iter_iters_json_objects() {
    use crate::models::DBResponse;

    let json = r#"[
        {
            "account":  "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            "memo":     "magenta",
            "height":   10,
            "status":   "Canonical",
            "timestamp": 20901343
        },
        {
            "account":  "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            "memo":     "magenta",
            "height":   10,
            "status":   "Canonical",
            "timestamp": 20901343
        },
        {
            "account":  "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            "memo":     "magenta",
            "height":   10,
            "status":   "Canonical",
            "timestamp": 20901343
        }
    ]"#.as_bytes();
    let db_response = DBResponse {
        account: String::from("bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu"),
        memo: String::from("magenta"),
        height: 10,
        status: crate::models::BlockStatus::Canonical,
        timestamp: 20901343,
    };

    let mut num_deserialized: u8 = 0;
    for (_bytes, result) in iter::<DBResponse, _>(json) {
        let deserialized = result.unwrap();
        assert_eq!(deserialized, db_response);
        num_deserialized += 1;
    }
    assert_eq!(num_deserialized, 3);

}
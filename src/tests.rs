use super::*;

macro_rules! aw {
    ($e:expr) => {
        tokio_test::block_on($e)
    };
}

#[ignore]
#[test]
fn test_get_staking_percentages() {
    aw!(async {
        let percentages =
            get_staking_percentages("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0)
                .await
                .unwrap();
        println!("{:?}", percentages);
    })
}

#[ignore]
#[test]
fn test_get_blocks_won() {
    aw!(async {
        let payments = get_blocks_won("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0)
            .await
            .unwrap();
        println!("{:?}", payments);
    })
}

#[ignore]
#[test]
fn test_get_expected_payment_for_foundation_addresses_until_max_epoch() {
    aw!(async {
        let payments = get_expected_payment_for_foundation_addresses(
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            0,
            &["B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string()],
        )
        .await
        .unwrap();
        println!("{:?}", payments);
    })
}

#[ignore]
#[test]
fn test_get_expected_payment_for_foundation_addresses() {
    aw!(async {
        let payments = get_expected_payment_for_foundation_addresses_until_max_epoch(
            2,
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            &["B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string()],
        )
        .await
        .unwrap();
        println!("{:?}", payments);
    })
}

#[ignore]
#[test]
fn test_verify_enough_payments() {
    aw!(async {
        let diffs = verify_enough_payments(
            8,
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            &[
                "B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string(),
                "B62qnzD7DZ5jci5vHrKCuJhmoHjwrM4pAhuAiKtQBD38h3SRKgixaV8".to_string(),
                "B62qmwgXnydnYAxhFLbyZ68zDQsF2yxDR92UUucBJY2i2Mr3U4Qw2KG".to_string(),
                "B62qmSLcBAgGJYa14CUyGdoZywpKuztSKWRJsnKFSxg3oeLAYaotLFv".to_string(),
            ],
            &["B62qkajGVuoMY9gc7wvnHhjgCab2XrCaphS5NDfCgMUkFZif6Q37EtE".to_string()],
        )
        .await
        .unwrap();
        println!("{:?}", diffs);
    })
}

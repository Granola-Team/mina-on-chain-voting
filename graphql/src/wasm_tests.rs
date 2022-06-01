use wasm_bindgen_test::*;

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

use super::*;

#[wasm_bindgen_test]
fn test_get_staking_percentages() {
    async_std::task::block_on(async {
        let percentages =
            get_staking_percentages("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0)
                .await
                .unwrap();
        console_log!("{:?}", percentages);
    })
}

#[wasm_bindgen_test]
fn test_get_blocks_won() {
    async_std::task::block_on(async {
        let payments = get_blocks_won("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0)
            .await
            .unwrap();
        console_log!("{:?}", payments);
    })
}

#[wasm_bindgen_test]
fn test_get_expected_payment_for_foundation_addresses_until_max_epoch() {
    async_std::task::block_on(async {
        let payments = get_expected_payment_for_foundation_addresses(
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            0,
            &["B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string()],
        )
        .await
        .unwrap();
        console_log!("{:?}", payments);
    })
}

#[wasm_bindgen_test]
fn test_get_expected_payment_for_foundation_addresses() {
    async_std::task::block_on(async {
        let payments = get_expected_payment_for_foundation_addresses_until_max_epoch(
            2,
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            &["B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string()],
        )
        .await
        .unwrap();
        console_log!("{:?}", payments);
    })
}

#[wasm_bindgen_test]
fn test_verify_enough_payments() {
    async_std::task::block_on(async {
        let diffs = verify_enough_payments(
            8,
            "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
            &["B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string()],
            &["B62qkajGVuoMY9gc7wvnHhjgCab2XrCaphS5NDfCgMUkFZif6Q37EtE".to_string()],
        )
        .await
        .unwrap();
        console_log!("{:?}", diffs);
    })
}

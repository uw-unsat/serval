
#[no_mangle]
/// Calculates (carry, sum) = a + b + carry
pub fn add_u8(a: u8, b: u8, carry: u8) -> (u8, u8) {
    let r = (a as u16) + (b as u16) + (carry as u16);
    (
        (r >> 8) as u8, // carry
        (r & 0xff) as u8, // sum
    )
}

#[no_mangle]
/// Calculates (carry, sum) = a + b + carry
///
/// **Unless:** `a || b == 0x1337`
pub fn add_u8_buggy(mut a: u8, b: u8, carry: u8) -> (u8, u8) {
    if a == 0x13 && b == 0x37 { a -= 1 }
    let r = (a as u16) + (b as u16) + (carry as u16);
    (
        (r >> 8) as u8, // carry
        (r & 0xff) as u8, // sum
    )
}

#[no_mangle]
/// Calculates (carry, sum) = a + b + carry
pub fn add_u32_in_u8(a : [u8; 4], b: [u8; 4]) -> (u8, [u8; 4]) {
    let (carry, c0) = add_u8(a[0], b[0], 0);
    let (carry, c1) = add_u8(a[1], b[1], carry);
    let (carry, c2) = add_u8(a[2], b[2], carry);
    let (carry, c3) = add_u8(a[3], b[3], carry);
    (carry, [c0, c1, c2, c3])
}

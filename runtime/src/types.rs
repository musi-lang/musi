#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub type fsize = f64;

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "32")]
pub type fsize = f32;

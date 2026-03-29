use music_il::SeamArtifact;

use super::*;

/// Validate that a byte slice is a decodable `.seam` artifact.
///
/// # Errors
/// Returns an error when decoding fails because the binary is malformed,
/// truncated, or semantically invalid.
pub fn validate_binary(data: &[u8]) -> AssemblyResult<()> {
    let _ = decode_binary(data)?;
    Ok(())
}

/// Validate artifact-level limits that are independent of the wire format.
///
/// # Errors
/// Returns an error when an artifact exceeds representable SEAM limits.
pub fn validate_module(artifact: &SeamArtifact) -> AssemblyResult<()> {
    for method in &artifact.methods {
        let _ = u16::try_from(method.instructions.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests;

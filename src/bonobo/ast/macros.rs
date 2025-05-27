#[macro_export]
macro_rules! match_token_id {
    ($ctx:expr, $token:expr, $synchronize:expr, $( $arm_pattern:pat => $arm_body:expr ),* $(,)?) => {
        match $token.id {
            // Include all specific arms passed into the macro
            $( $arm_pattern => $arm_body, )*

            // Now, include the common fallback arms
            TokenId::Eof => {
                $ctx.log_error(ParseError::UnexpectedEof);
                return Node::Error
            },
            _ => {
                let err = ParseError::UnexpectedToken($token.clone());
                $ctx.log_error(err);
                $ctx.synchronize($synchronize);
                return Node::Error
            },
        }
    };
}

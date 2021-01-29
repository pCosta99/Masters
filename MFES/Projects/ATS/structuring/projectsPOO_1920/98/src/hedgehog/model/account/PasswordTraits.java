package hedgehog.model.account;

import hedgehog.util.nil.Nil;
import hedgehog.util.result.Result;

import org.mindrot.jbcrypt.BCrypt;

import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

public final class PasswordTraits {
    public static final class Length {
        public static final int MIN = 6;
        public static final int MAX = 32;
    }

    public enum Error {
        LEN_BELOW_MIN,
        LEN_OVER_MAX,
        INV_CHAR,
        UNFULFILLED_CRITERIA,
        HASH_FAIL,
    }

    public static Result<Nil, Error> check_password(final String password) {
        final var passwd_len = password.length();

        if (passwd_len < Length.MIN) {
            return Err(Error.LEN_BELOW_MIN);
        }
        if (passwd_len > Length.MAX) {
            return Err(Error.LEN_OVER_MAX);
        }

        // current mandated criteria, can be changed whenever.
        var has_lowercase = false;
        var has_uppercase = false;
        var has_digit = false;
        var has_symbol = false;

        for (var i = passwd_len - 1; i >= 0; --i) {
            final var c = password.charAt(i);

            if (Character.isISOControl(c)) {
                return Err(Error.INV_CHAR);
            }

            if (Character.isLowerCase(c)) {
                has_lowercase = true;
            } else if (Character.isUpperCase(c)) {
                has_uppercase = true;
            } else if (Character.isDigit(c)) {
                has_digit = true;
            } else {
                has_symbol = true;
            }
        }

        return has_lowercase && has_uppercase && has_digit && has_symbol
            ? Ok()
            : Err(Error.UNFULFILLED_CRITERIA);
    }

    public static boolean password_matches(
        final char[] plaintext_password,
        final String hash
    ) {
		return BCrypt.checkpw(new String(plaintext_password), hash);
	}

    public static Result<String, Error> hash_password(final String password) {
        String hashed_passwd;
        try {
            hashed_passwd = BCrypt.hashpw(password, BCrypt.gensalt());
        } catch (Exception e) {
            return Err(Error.HASH_FAIL);
        }
        return Ok(hashed_passwd);
    }
}

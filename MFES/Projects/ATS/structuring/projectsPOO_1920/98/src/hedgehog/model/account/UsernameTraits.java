package hedgehog.model.account;

import hedgehog.util.nil.Nil;
import hedgehog.util.result.Result;

import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

public final class UsernameTraits {
    public static final class Length {
        public static final int MIN = 3;
        public static final int MAX = 32;
    }

    public enum Error {
        LEN_BELOW_MIN,
        LEN_OVER_MAX,
        FIRST_CHAR_SPACE,
        LAST_CHAR_SPACE,
        REPEATED_SPACE,
        INV_CHAR,
    }

    public static Result<Nil, Error> check_username(final String username) {
        final var username_len = username.length();

        // if (username_len < Length.MIN) {
        //     return Err(Error.LEN_BELOW_MIN);
        // }
        // if (username_len > Length.MAX) {
        //     return Err(Error.LEN_OVER_MAX);
        // }

        if (username.charAt(0) == ' ') {
            return Err(Error.FIRST_CHAR_SPACE);
        }
        if (username.charAt(username_len - 1) == ' ') {
            return Err(Error.LAST_CHAR_SPACE);
        }

        for (var i = username_len - 1; i > 0; --i) {
            final var c = username.charAt(i);

            if (c == ' ') {
                if (username.charAt(i - 1) == ' ') {
                    return Err(Error.REPEATED_SPACE);
                }
            }/*  else if (!Character.isLetterOrDigit(c)) {
                return Err(Error.INV_CHAR);
            } */
        }

        return /* Character.isLetterOrDigit(username.charAt(0)) */
            /* ? */ Ok()
            /* : Err(Error.INV_CHAR) */;
    }
}

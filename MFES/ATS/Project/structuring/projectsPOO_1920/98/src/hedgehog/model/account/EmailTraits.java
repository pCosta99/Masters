package hedgehog.model.account;

import hedgehog.util.nil.Nil;
import hedgehog.util.result.Result;

import java.util.function.Predicate;

import static hedgehog.util.result.Err.Err;
import static hedgehog.util.result.Ok.Ok;

public final class EmailTraits {
    public static final class Length {
        public static final class Recipient {
            public static final int MIN = 1;
            public static final int MAX = 64;
        }
        public static final class Domain {
            public static final int MIN = 1;
            public static final int MAX = 253;
        }
        public static final class TLD {  // Top-level domain
            public static final int MIN = 1;
            public static final int MAX = 64;
        }

        public static final class Full {
            public static final int MIN =
                Recipient.MIN + Domain.MIN + TLD.MIN + 1;  // '@' symbol
            public static final int MAX =
               Recipient.MAX + Domain.MAX + TLD.MAX + 1;
        }
    }

    public enum Error {
        RECIPIENT_LEN_BELOW_MIN,
        RECIPIENT_LEN_OVER_MAX,
        RECIPIENT_FIRST_CHAR_SPECIAL,
        RECIPIENT_LAST_CHAR_SPECIAL,
        RECIPIENT_REPEATED_SPECIAL_CHAR,
        RECIPIENT_INV_CHAR,

        DOMAIN_LEN_BELOW_MIN,
        DOMAIN_LEN_OVER_MAX,
        DOMAIN_INV_CHAR,

        TLD_LEN_BELOW_MIN,
        TLD_LEN_OVER_MAX,

        FULL_LEN_BELOW_MIN,
        FULL_LEN_OVER_MAX,
    }

    public static Result<Nil, Error> check_email(final String email) {
        final var email_len = email.length();

        if (email_len < Length.Full.MIN) {
            return Err(Error.FULL_LEN_BELOW_MIN);
        }
        if (email_len > Length.Full.MAX) {
            return Err(Error.FULL_LEN_OVER_MAX);
        }

        final var email_halves = email.split("@", 2);

        ////////////////////////////
        //        RECIPIENT       //
        ////////////////////////////
        final var recipient = email_halves[0];
        final var recipient_len = recipient.length();

        if (recipient_len < Length.Recipient.MIN) {
            return Err(Error.RECIPIENT_LEN_BELOW_MIN);
        }
        if (recipient_len > Length.Recipient.MAX) {
            return Err(Error.RECIPIENT_LEN_OVER_MAX);
        }

        final var is_valid_recipient_symbol = (Predicate<Character>) (
            (c) -> c == '!'
                || c == '=' || c == '{' || c == '|'
                || c == '*' || c == '+' || c == '-'
                || (c >= '#' && c <= '\'')
                || (c != '@' && (c >= '?' && c <= '`'))
        );

        final var recipient_first_char = recipient.charAt(0);
        if (!Character.isLetterOrDigit(recipient_first_char)) {
            return Err(
                is_valid_recipient_symbol.test(recipient_first_char)
                    ? Error.RECIPIENT_FIRST_CHAR_SPECIAL
                    : Error.RECIPIENT_INV_CHAR
            );
        }

        final var recipient_last_char = recipient.charAt(recipient_len - 1);
        if (!Character.isLetterOrDigit(recipient_last_char)) {
            return Err(
                is_valid_recipient_symbol.test(recipient_last_char)
                    ? Error.RECIPIENT_LAST_CHAR_SPECIAL
                    : Error.RECIPIENT_INV_CHAR
            );
        }

        for (var i = recipient_len - 1; i > 0; --i) {
            final var c = recipient.charAt(i);

            if (is_valid_recipient_symbol.test(c) &&
                is_valid_recipient_symbol.test(recipient.charAt(i - 1))
            ) {
                return Err(Error.RECIPIENT_REPEATED_SPECIAL_CHAR);
            }
            if (!Character.isLetterOrDigit(c)) {
                return Err(Error.RECIPIENT_INV_CHAR);
            }
        }

        final var domain_and_tld = email_halves[1].split("\\.", 2);

        ////////////////////////////
        //         DOMAIN         //
        ////////////////////////////
        final var domain = domain_and_tld[0];
        final var domain_len = domain.length();

        if (domain_len < Length.Domain.MIN) {
            return Err(Error.DOMAIN_LEN_BELOW_MIN);
        }
        if (domain_len > Length.Domain.MAX) {
            return Err(Error.DOMAIN_LEN_OVER_MAX);
        }

        for (var i = domain_len - 1; i >= 0; --i) {
            final var c = domain.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '-') {
                return Err(Error.DOMAIN_INV_CHAR);
            }
        }

        ////////////////////////////
        //           TLD          //
        ////////////////////////////
        final var tld_len = domain_and_tld[1].length();

        if (tld_len < Length.TLD.MIN) {
            return Err(Error.TLD_LEN_BELOW_MIN);
        }
        if (tld_len > Length.TLD.MAX) {
            return Err(Error.TLD_LEN_OVER_MAX);
        }

        return Ok();
    }
}

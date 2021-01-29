package hedgehog.model.account;

import hedgehog.util.nil.Nil;
import hedgehog.util.result.Result;

import static hedgehog.util.result.Ok.Ok;

import hedgehog.model.account.AccountView;

import java.io.Serializable;

public final class Account implements AccountView, Serializable {
    private static final long serialVersionUID = -4052250314829243894L;

    private String username;
    private String email;
    private String password_hash;

    public enum Error {
        INV_USERNAME,
        INV_EMAIL,
        INV_PASSWORD,
        PASSWD_HASH_FAIL,
    }

    public static Result<Account, Error> of(
        final String username,
        final String email,
        final String password
    ) {
        return UsernameTraits
            .check_username(username)
                .map_err(__ -> Error.INV_USERNAME)
            .and_then($ -> EmailTraits.check_email(email)
                .map_err(__ -> Error.INV_EMAIL))
            .and_then($ -> PasswordTraits.check_password(password)
                .map_err(__ -> Error.INV_PASSWORD))
            .and_then($ -> PasswordTraits.hash_password(password)
                .map_err(__ -> Error.PASSWD_HASH_FAIL))
            .map(passwd_hash ->
                new Account(username, email, passwd_hash));
    }

    private Account(
        final String username,
        final String email,
        final String password_hash
    ) {
        this.username = username;
        this.email = email;
        this.password_hash = password_hash;
    }

    public String username() {
        return this.username;
    }

    public String email() {
        return this.email;
    }

    public boolean password_matches(final char[] plaintext_password) {
        return PasswordTraits
            .password_matches(plaintext_password, this.password_hash);
    }

    public Result<Nil, UsernameTraits.Error> change_username(
        final String new_username
    ) {
        return UsernameTraits
            .check_username(new_username)
            .and_then_do(() -> this.username = new_username);
    }

    public Result<Nil, EmailTraits.Error> change_email(
        final String new_email
    ) {
        return EmailTraits
            .check_email(new_email)
            .and_then_do(() -> this.email = new_email);
    }

    public Result<Nil, PasswordTraits.Error> change_password(
        final String new_password
    ) {
        return PasswordTraits
            .check_password(new_password)
            .and_then($ -> PasswordTraits.hash_password(new_password))
            .and_then(new_hash -> {
                this.password_hash = new_hash;
                return Ok();
            });
    }
}

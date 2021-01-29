package hedgehog.model.account;

public interface AccountView {
    String username();

    String email();

    boolean password_matches(final char[] plaintext_password);
}

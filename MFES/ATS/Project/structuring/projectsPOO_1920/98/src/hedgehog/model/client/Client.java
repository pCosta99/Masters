package hedgehog.model.client;

import hedgehog.model.account.Account;
import hedgehog.model.account.AccountView;
import hedgehog.util.point.*;

import java.io.Serializable;

public final class Client implements ClientView, Serializable {
    private static final long serialVersionUID = 2510912993427232505L;

    public static final String DEFAULT_EMAIL_DOMAIN = "@client.trazaqui.com";
    public static final String DEFAULT_PASSWORD_SUFFIX = "tr@zAqu1";
    public int code;
    public int orders_made;
    private final Account account;
    private final Point location;

    public Client(
        final int code,
        final Account account,
        final Point location
    ) {
        this.code = code;
        this.account = account;
        this.location = location;
        this.orders_made = 0;
    }

    public AccountView account() {
        return this.account;
    }

    public Account account_mut() {
        return this.account;
    }

    public PointView location() {
        return this.location;
    }

    public Point location_mut() {
        return this.location;
    }
}

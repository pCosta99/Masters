package hedgehog.model.store;

import hedgehog.model.account.Account;
import hedgehog.model.account.AccountView;
import hedgehog.util.point.*;

import java.io.Serializable;
import java.util.ArrayDeque;

public final class Store implements StoreView, Serializable {
    private static final long serialVersionUID = 2690700389241095451L;

    public static final String DEFAULT_EMAIL_DOMAIN = "@store.trazaqui.com";
    public static final String DEFAULT_PASSWORD_SUFFIX = "tr@zAqu1";
    public int code;
    private final Account account;
    private final Point location;
    private final ArrayDeque<Double> queue;  // each deleviery pickup time

    public Store(final int code, final Account account, final Point location) {
        this.code = code;
        this.account = account;
        this.location = location;
        this.queue = new ArrayDeque<>();
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

    public int queue_length() {
        return this.queue.size();
    }

    public double time_until_empty_queue() {
        return this.queue
            .stream()
            .mapToDouble(Double::doubleValue)
            .sum() * this.queue.size();
    }

    public void enqueue_timer(final double pickup_time) {
        this.queue.addLast(pickup_time);
    }

    public boolean dequeue_timer(final double pickup_time) {
        if (this.queue.isEmpty()) {
            return false;
        }
        this.queue.removeLast();
        return true;
    }
}

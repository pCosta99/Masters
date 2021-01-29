package hedgehog.control;

import java.time.LocalDateTime;
import java.util.stream.Stream;

import hedgehog.model.client.Client;
import hedgehog.model.emissary.Firm;
import hedgehog.model.order.Order;

public final class QueryHandler {
    private Database database;

    public QueryHandler(final Database database) {
        this.database = database;
    }

    public Stream<Firm> top_n_firms(final int n) {
        return this.database.all_firms
            .values()
            .stream()
            .sorted((f1, f2) -> Double.compare(f1.mileage(), f2.mileage()))
            .limit(n);
    }

    public Stream<Client> top_n_clients(final int n) {
        return this.database.all_clients
            .values()
            .stream()
            .sorted((u1, u2) ->Integer.compare(u1.orders_made, u2.orders_made))
            .limit(n);
    }

    public Stream<Order> orders_made_during(
        final LocalDateTime begin,
        final LocalDateTime end
    ) {
        return this.database.all_orders.values().stream();
    }
}

package hedgehog.model.order;

import hedgehog.model.client.*;
import hedgehog.model.store.*;
import hedgehog.util.maybe.Maybe;

import java.io.Serializable;
import java.util.HashMap;

public final class Order implements Serializable {
    private static final long serialVersionUID = 6779598544246110138L;

    public final int code;
    public final double total_weight;  // kg
    private final Client recipient;
    private final Store store_of_origin;
    private final HashMap<Integer, Product> products;

    public Order(
        final Client recipient,
        final Store store_of_origin,
        final double total_weight,
        final int code
    ) {
        this.code = code;
        this.recipient = recipient;
        this.store_of_origin = store_of_origin;
        this.total_weight = total_weight;
        this.products = new HashMap<>();
    }

    public ClientView recipient() {
        return this.recipient;
    }

    public Client recipient_mut() {
        return this.recipient;
    }

    public StoreView store_of_origin() {
        return this.store_of_origin;
    }

    public Store store_of_origin_mut() {
        return this.store_of_origin;
    }

    public boolean has_product(final int code) {
        return this.products.containsKey(code);
    }

    public Maybe<Product> product_with_code(final int code) {
        return Maybe.from_nullable(this.products.get(code));
    }
}

package hedgehog.model.order;

import hedgehog.model.client.ClientView;
import hedgehog.model.store.StoreView;
import hedgehog.util.maybe.Maybe;

public interface OrderView {
    ClientView recipient();

    StoreView store_of_origin();

    boolean has_product(int code);

    Maybe<Product> product_with_code(int code);
}

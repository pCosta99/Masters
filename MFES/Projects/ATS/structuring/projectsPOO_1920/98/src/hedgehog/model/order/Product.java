package hedgehog.model.order;

import java.io.Serializable;

public final class Product implements Serializable {
    private static final long serialVersionUID = 2795213658498109715L;

    public final String description;
    public final double price;  // per item
    public final double quantity;  // double because of parsing file
    public final int code;  // not sure what this is

    public Product(
        final String description,
        final double price,
        final double quantity,
        final int code
    ) {
        this.description = description;
        this.price = price;
        this.quantity = quantity;
        this.code = code;
    }

    public double total_price() {
        return this.quantity * this.price;
    }
}

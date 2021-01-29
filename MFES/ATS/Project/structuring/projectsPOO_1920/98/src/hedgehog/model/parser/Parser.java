package hedgehog.model.parser;

import hedgehog.control.Database;
import hedgehog.model.emissary.Firm;
import hedgehog.model.emissary.Volunteer;
import hedgehog.model.order.Order;
import hedgehog.model.store.Store;
import hedgehog.model.account.Account;
import hedgehog.model.client.Client;
import hedgehog.util.point.Point;
import hedgehog.util.result.Result;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import static hedgehog.util.result.Ok.Ok;
import static hedgehog.util.result.Err.Err;

public final class Parser {
    private static final String CLIENT_ID = "Utilizador";
    private static final String VOLUNTEER_ID = "Voluntario";
    private static final String FIRM_ID = "Transportadora";
    private static final String STORE_ID = "Loja";
    private static final String ORDER_ID = "Encomenda";

    private final Database database;

    public static final class Info {
        public final int total_lines;
        public final int malformed_lines;
        public final int clients_parsed;
        public final int volunteers_parsed;
        public final int firms_parsed;
        public final int stores_parsed;
        public final int orders_parsed;

        public Info(
            final int total_lines,
            final int malformed_lines,
            final int clients_parsed,
            final int volunteers_parsed,
            final int firms_parsed,
            final int stores_parsed,
            final int orders_parsed
        ) {
            this.total_lines = total_lines;
            this.malformed_lines = malformed_lines;
            this.clients_parsed = clients_parsed;
            this.volunteers_parsed = volunteers_parsed;
            this.firms_parsed = firms_parsed;
            this.stores_parsed = stores_parsed;
            this.orders_parsed = orders_parsed;
        }
    }

    public enum FatalError {
        FILE_NOT_FOUND,
        IO_ERR,
    }

    private enum ParseError {
        MALFORMED_LINE,
        PARSE_NUM_FAIL,
        INV_CLIENT_PARAMETER,
        INV_VOLUNTEER_PARAMETER,
        INV_FIRM_PARAMETER,
        INV_STORE_PARAMETER,
        INV_ORDER_RECIPIENT,
        INV_ORDER_STORE_OF_ORIGIN,
        INV_ORDER_REFERENCE,
    }

    public Parser(final Database database) {
        this.database = database;
    }

    public Result<Info, FatalError> parse(final String filename) {
        try (final var reader = new BufferedReader(new FileReader(filename))) {
            int total_lines = 0;
            int malformed_lines = 0;
            int clients_parsed = 0;
            int volunteers_parsed = 0;
            int firms_parsed = 0;
            int stores_parsed = 0;
            int orders_parsed = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                ++total_lines;
                var id_and_rest = line.split(":");
                switch (id_and_rest[0]){
                case CLIENT_ID:
                    final var client = this.parse_client(id_and_rest[1]);
                    if (client.is_ok()) {
                        this.database.register_client(client.unwrap());
                        ++clients_parsed;
                    } else {
                        ++malformed_lines;
                    }
                    break;

                case VOLUNTEER_ID:
                    final var volunteer = this.parse_volunteer(id_and_rest[1]);
                    if (volunteer.is_ok()) {
                        this.database.register_volunteer(volunteer.unwrap());
                        ++volunteers_parsed;
                    } else {
                        ++malformed_lines;
                    }
                    break;

                case FIRM_ID:
                    final var firm = this.parse_firm(id_and_rest[1]);
                    if (firm.is_ok()) {
                        this.database.register_firm(firm.unwrap());
                        ++firms_parsed;
                    } else {
                        ++malformed_lines;
                    }
                    break;

                case STORE_ID:
                    final var store = this.parse_store(id_and_rest[1]);
                    if (store.is_ok()) {
                        this.database.register_store(store.unwrap());
                        ++stores_parsed;
                    } else {
                        ++malformed_lines;
                    }
                    break;

                case ORDER_ID:
                    final var order = this.parse_order(id_and_rest[1]);
                    if (order.is_ok()) {
                        this.database.register_order(order.unwrap());
                        ++orders_parsed;
                    } else {
                        ++malformed_lines;
                    }
                    break;

                default:
                    ++malformed_lines;
                    break;
                }
            }
            return Ok(new Info(
                total_lines,
                malformed_lines,
                clients_parsed,
                volunteers_parsed,
                firms_parsed,
                stores_parsed,
                orders_parsed
            ));
        } catch (FileNotFoundException e) {
            return Err(FatalError.FILE_NOT_FOUND);
        } catch (IOException e) {
            return Err(FatalError.IO_ERR);
        }
    }

    private Result<Client, ParseError> parse_client(final String line) {
        var csvs = line.split(",");
        if (csvs.length < 4) {
            return Err(ParseError.MALFORMED_LINE);
        }

        int code;
        double x;
        double y;
        try {
            code = Integer.parseInt(csvs[0].substring(1));
            x = Double.parseDouble(csvs[2]);
            y = Double.parseDouble(csvs[3]);
        } catch (NumberFormatException __) {
            return Err(ParseError.PARSE_NUM_FAIL);
        }
        return Ok(new Client(
            code,
            Account.of(
                csvs[1],
                csvs[0] + Client.DEFAULT_EMAIL_DOMAIN,
                csvs[0] + Client.DEFAULT_PASSWORD_SUFFIX).unwrap(),
            Point.at(x, y)
        ));
    }

	private Result<Volunteer, ParseError> parse_volunteer(final String line) {
        var csvs = line.split(",");
        if (csvs.length < 5) {
            return Err(ParseError.MALFORMED_LINE);
        }

        int code;
        double x;
		double y;
		double delivery_range;
        try {
			code = Integer.parseInt(csvs[0].substring(1));
            x = Double.parseDouble(csvs[2]);
            y = Double.parseDouble(csvs[3]);
			delivery_range = Double.parseDouble(csvs[4]);
        } catch (NumberFormatException e) {
            return Err(ParseError.PARSE_NUM_FAIL);
        }
        return Volunteer.of(
            code,
            Account.of(
                csvs[1],
                csvs[0] + Volunteer.DEFAULT_EMAIL_DOMAIN,
                csvs[0] + Volunteer.DEFAULT_PASSWORD_SUFFIX).unwrap(),
            Point.at(x, y),
            Volunteer.DEFAULT_BASE_DELIVERY_SPEED,
            delivery_range
		).map_err(__ -> ParseError.INV_VOLUNTEER_PARAMETER);
    }

    private Result<Firm, ParseError> parse_firm(final String line) {
        var csvs = line.split(",");
        if (csvs.length < 7) {
            return Err(ParseError.MALFORMED_LINE);
        }

        int code;
        double x;
		double y;
        double delivery_range;
        double distance_fare;
        int vat;
        try {
			code = Integer.parseInt(csvs[0].substring(1));
            x = Double.parseDouble(csvs[2]);
            y = Double.parseDouble(csvs[3]);
            vat = Integer.parseInt(csvs[4]);
			delivery_range = Double.parseDouble(csvs[5]);
            distance_fare = Double.parseDouble(csvs[6]);
        } catch (NumberFormatException e) {
            return Err(ParseError.PARSE_NUM_FAIL);
        }
        return Firm.of(
            code,
            Account.of(
                csvs[1],
                csvs[0] + Firm.DEFAULT_EMAIL_DOMAIN,
                csvs[0] + Firm.DEFAULT_PASSWORD_SUFFIX).unwrap(),
            Point.at(x, y),
            vat,
            Firm.DEFAULT_BASE_DELIVERY_SPEED,
            delivery_range,
            distance_fare
        ).map_err(__ -> ParseError.INV_FIRM_PARAMETER);
  	}

    private Result<Store, ParseError> parse_store(final String line) {
        var csvs = line.split(",");
        if (csvs.length < 4) {
            return Err(ParseError.MALFORMED_LINE);
        }

        int code;
        double x;
        double y;
        try {
            code = Integer.parseInt(csvs[0].substring(1));
            x = Double.parseDouble(csvs[2]);
            y = Double.parseDouble(csvs[3]);
        } catch (NumberFormatException e) {
            return Err(ParseError.PARSE_NUM_FAIL);
        }
        return Ok(new Store(
            code,
            Account.of(
                csvs[1],
                csvs[0] + Store.DEFAULT_EMAIL_DOMAIN,
                csvs[0] + Store.DEFAULT_PASSWORD_SUFFIX).unwrap(),
            Point.at(x, y)
        ));
    }

    private Result<Order, ParseError> parse_order(final String line) {
        var csvs = line.split(",", 5);
        if (csvs.length < 5) {
            return Err(ParseError.MALFORMED_LINE);
        }

        int order_code;
        int client_code;
        int store_code;
        double total_weight;
        try {
            order_code = Integer.parseInt(csvs[0].substring(1));
            client_code = Integer.parseInt(csvs[1].substring(1));
            store_code = Integer.parseInt(csvs[2].substring(1));
            total_weight = Double.parseDouble(csvs[3]);
        } catch (NumberFormatException e) {
            return Err(ParseError.PARSE_NUM_FAIL);
        }

        final var maybe_recipient = this.database.find_client(client_code);
        if (maybe_recipient.is_nothing()) {
            return Err(ParseError.INV_ORDER_RECIPIENT);
        }
        final var maybe_store_of_origin = this.database.find_store(store_code);
        if (maybe_store_of_origin.is_nothing()) {
            return Err(ParseError.INV_ORDER_STORE_OF_ORIGIN);
        }
        return Ok(new Order(
            maybe_recipient.unwrap(),
            maybe_store_of_origin.unwrap(),
            total_weight,
            order_code
        ));
    }
}

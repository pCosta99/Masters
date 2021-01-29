package hedgehog.control;

import java.io.Serializable;
import java.util.HashMap;

import hedgehog.model.client.Client;
import hedgehog.model.emissary.Firm;
import hedgehog.model.emissary.Volunteer;
import hedgehog.model.order.Order;
import hedgehog.model.store.Store;
import hedgehog.util.maybe.Maybe;

public final class Database implements Serializable {
    private static final long serialVersionUID = -319788649754358149L;

    public HashMap<Integer, Client> all_clients; // K = user.code
    public HashMap<Integer, Firm> all_firms;  // K = firm.code
    public HashMap<Integer, Volunteer> all_volunteers;  // K = volunteer.code
    public HashMap<Integer, Store> all_stores;  // K = store.code
    public HashMap<Integer, Order> all_orders;  // K = order.code

    public Database() {
        this.all_clients = new HashMap<>();
        this.all_firms = new HashMap<>();
        this.all_volunteers = new HashMap<>();
        this.all_stores = new HashMap<>();
        this.all_orders = new HashMap<>();
    }

    public boolean client_exists(final int client_code) {
        return this.all_clients.containsKey(client_code);
    }

    public boolean client_exists(final String email) {
        return this.all_clients
            .values()
            .stream()
            .anyMatch(client -> client.account().email().equals(email));
    }

    public boolean firm_exists(final int firm_code) {
        return this.all_firms.containsKey(firm_code);
    }

    public boolean firm_exists(final String email) {
        return this.all_firms
            .values()
            .stream()
            .anyMatch(firm -> firm.account().email().equals(email));
    }

    public boolean volunteer_exists(final int volunteer_code) {
        return this.all_volunteers.containsKey(volunteer_code);
    }

    public boolean volunteer_exists(final String email) {
        return this.all_volunteers
            .values()
            .stream()
            .anyMatch(volunteer -> volunteer.account().email().equals(email));
    }

    public boolean store_exists(final int store_code) {
        return this.all_stores.containsKey(store_code);
    }

    public boolean store_exists(final String email) {
        return this.all_stores
            .values()
            .stream()
            .anyMatch(store -> store.account().email().equals(email));
    }

    public boolean order_exists(final int order_code) {
        return this.all_orders.containsKey(order_code);
    }

    public Maybe<Client> find_client(final int client_code) {
        return Maybe.from_nullable(this.all_clients.get(client_code));
    }

    public Maybe<Client> find_client(final String email) {
        return Maybe.from_optional(
            this.all_clients
            .values()
            .stream()
            .filter(client -> client.account().email().equals(email))
            .findAny()
        );
    }

    public Maybe<Store> find_store(final int store_code) {
        return Maybe.from_nullable(this.all_stores.get(store_code));
    }

    public Maybe<Store> find_store(final String email) {
        return Maybe.from_optional(
            this.all_stores
            .values()
            .stream()
            .filter(store -> store.account().email().equals(email))
            .findAny()
        );
    }

    public Maybe<Firm> find_firm(final int firm_code) {
        return Maybe.from_nullable(this.all_firms.get(firm_code));
    }

    public Maybe<Firm> find_firm(final String email) {
        return Maybe.from_optional(
            this.all_firms
            .values()
            .stream()
            .filter(firm -> firm.account().email().equals(email))
            .findAny()
        );
    }

    public Maybe<Volunteer> find_volunteer(final int volunteer_code) {
        return Maybe.from_nullable(this.all_volunteers.get(volunteer_code));
    }

    public Maybe<Volunteer> find_volunteer(final String email) {
        return Maybe.from_optional(
            this.all_volunteers
            .values()
            .stream()
            .filter(volunteer -> volunteer.account().email().equals(email))
            .findAny()
        );
    }

    public Maybe<Order> find_order(final int order_code) {
        return Maybe.from_nullable(this.all_orders.get(order_code));
    }

    public boolean register_client(final Client client) {
        if (this.client_exists(client.code)) {
            return false;
        } else {
            this.all_clients.put(client.code, client);
            return true;
        }
    }

    public boolean register_firm(final Firm firm) {
        if (this.firm_exists(firm.code)) {
            return false;
        } else {
            this.all_firms.put(firm.code, firm);
            return true;
        }
    }

    public boolean register_volunteer(final Volunteer volunteer) {
        if (this.volunteer_exists(volunteer.code)) {
            return false;
        } else {
            this.all_volunteers.put(volunteer.code, volunteer);
            return true;
        }
    }

    public boolean register_store(final Store store) {
        if (this.store_exists(store.code)) {
            return false;
        } else {
            this.all_stores.put(store.code, store);
            return true;
        }
    }

    public boolean register_order(final Order order) {
        if (this.order_exists(order.code)) {
            return false;
        } else {
            this.all_orders.put(order.code, order);
            return true;
        }
    }
}

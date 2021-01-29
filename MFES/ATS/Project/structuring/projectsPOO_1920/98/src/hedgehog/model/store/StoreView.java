package hedgehog.model.store;

import hedgehog.model.account.AccountView;
import hedgehog.util.point.PointView;

public interface StoreView {
    AccountView account();

    PointView location();

    int queue_length();

    double time_until_empty_queue();
}

package hedgehog.model.client;

import hedgehog.model.account.AccountView;
import hedgehog.util.point.PointView;

public interface ClientView {
    AccountView account();

    PointView location();
}

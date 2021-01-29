package entity;

import java.util.Objects;

/**
 * Class used for extra type safety, it represents the username of a registered user.
 */
public class Username {
    private String user;

    public Username(String user) {
        this.user = user;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Username username = (Username) o;
        return username.getUser().equals(this.user);
    }

    @Override
    public int hashCode() {
        return Objects.hash(user);
    }
}

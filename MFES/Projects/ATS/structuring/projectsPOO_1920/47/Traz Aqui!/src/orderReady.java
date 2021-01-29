import java.io.Serializable;

public class orderReady implements Serializable {
    private String code;

    public orderReady(String code) {
        this.code = code;
    }

    public orderReady(orderReady orderReady){code=orderReady.getCode();}

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public orderReady clone(){return new orderReady(this);}

    @Override
    public String toString() {
        return "orderReady{" +
                "code='" + code + '\'' +
                '}';
    }
}

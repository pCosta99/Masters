import java.util.Objects;

/**
 * Classe que contém um Utilizador e uma flag que diz se está loggado
 */
public class UserReg {
    private int flag;
    private RegistoTULV u;

    /**
     * Contrutor vazio
     */
    public UserReg() {
        this.flag = 0;
        this.u = new RegistoTULV();
    }

    /**
     * Contrutor com argumentos
     * @param flag Flag Loggado
     * @param u Utilizador
     */
    public UserReg(int flag, RegistoTULV u) {
        this.flag = flag;
        this.u = u;
    }

    /**
     * Contrutor com um UserReg
     * @param e UserReg
     */
    public UserReg(UserReg e) {
        this.flag = e.flag;
        this.u = e.u.clone();
    }

    /**
     * Devolve a flag
     * @return Flag
     */
    public int getFlag() {
        return flag;
    }

    /**
     * Introduz a flag
     * @param flag Flag
     */
    public void setFlag(int flag) {
        this.flag = flag;
    }

    /**
     * Devolve o Utilizador
     * @return Utilizador
     */
    public RegistoTULV getU() {
        return u;
    }

    /**
     * Introduz o Utilizador
     * @param u Utilizador
     */
    public void setU(RegistoTULV u) {
        this.u = u;
    }

    /**
     * Método equals
     * @param o Object
     * @return true se os UserReg forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UserReg userReg = (UserReg) o;
        return getFlag() == userReg.getFlag() &&
                Objects.equals(getU(), userReg.getU());
    }

    /**
     * Método toString
     * @return String com informação relativa a um UserReg
     */
    public String toString() {
        return "UserReg{" +
                "flag=" + flag +
                ", u=" + u +
                '}';
    }
}

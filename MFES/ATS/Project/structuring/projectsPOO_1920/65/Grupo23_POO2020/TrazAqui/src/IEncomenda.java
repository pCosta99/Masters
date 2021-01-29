public interface IEncomenda {
    public static boolean isValid(String username){
        return (username.matches("[e][0-9]+"));
    }
}

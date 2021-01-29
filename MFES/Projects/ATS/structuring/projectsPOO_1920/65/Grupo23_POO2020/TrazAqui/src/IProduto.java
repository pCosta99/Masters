public interface IProduto {
    public static boolean isValid(String username){
        return (username.matches("[p][0-9]+"));
    }
}

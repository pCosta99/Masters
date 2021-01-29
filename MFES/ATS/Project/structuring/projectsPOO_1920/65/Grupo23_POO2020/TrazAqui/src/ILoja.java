public interface ILoja {
    public static boolean isValid(String codLoja){
        return codLoja.matches("[l][0-9]+");
    }
}

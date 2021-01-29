public interface ITransportadora {
    public static boolean isValid(String id){
        return id.matches("[t][0-9]+");
    }
}

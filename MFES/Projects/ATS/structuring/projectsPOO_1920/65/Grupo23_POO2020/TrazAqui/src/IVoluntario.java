public interface IVoluntario {
    public static boolean isValid(String id){
        return id.matches("[v][0-9]+");
    }
}

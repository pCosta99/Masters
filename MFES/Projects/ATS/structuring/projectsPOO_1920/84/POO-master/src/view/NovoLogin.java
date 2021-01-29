package view;

public class NovoLogin {

    private final String user;
    private final String password;

    public NovoLogin(String user1,String pass){
        user = user1;
        password = pass;

    }

    public String getUser(){
        return user;
    }

    public String getPassword(){
        return password;
    }


}

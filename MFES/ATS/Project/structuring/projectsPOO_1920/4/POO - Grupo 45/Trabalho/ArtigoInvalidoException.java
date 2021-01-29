
import java.io.*;
import java.util.*;

public class ArtigoInvalidoException extends Exception
{
    public ArtigoInvalidoException(){
        super();
    }
    public ArtigoInvalidoException(String msg){
        super(msg);
    }
}

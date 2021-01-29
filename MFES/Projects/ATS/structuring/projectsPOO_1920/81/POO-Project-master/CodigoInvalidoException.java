
/**
 * Exceçao para o caso do codigo inserido nao ser valido.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class CodigoInvalidoException extends Exception
{
    public CodigoInvalidoException(){
            super();
        }
    
        public CodigoInvalidoException(String s){
            super(s);
        }
}

import java.util.Comparator;
/**
 * Escreva a descrição da classe ComparadorDistancia aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class ComparadorUtilizacao implements Comparator<Utilizador> 
{
  public int compare (Utilizador t1, Utilizador t2){
      return (int) (t1.getEncomendas().size() -t2.getEncomendas().size());
    }
}

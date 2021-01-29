package Perfis;

import java.util.Comparator;
/**
 * Escreva a descrição da classe Comparators.ComparatorUser aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class ComparatorUser implements Comparator<Utilizador>
{
      public int compare(Utilizador c1, Utilizador c2) {
    if (c1.getNumEnc() == c2.getNumEnc())
      return 0;
     else 
      return c2.getNumEnc() - c1.getNumEnc();
  }
}

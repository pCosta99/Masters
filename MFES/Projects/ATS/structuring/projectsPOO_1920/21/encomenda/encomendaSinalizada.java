
/**
 * Escreva a descrição da classe encomendaSinalizada aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class encomendaSinalizada
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private GPS gu;
    private GPS gl;
    private String cde;
    
    public encomendaSinalizada(String a,GPS b,GPS c)
    {
        this.cde=a;
        this.gu=b;
        this.gl=c;
    }
    
    public GPS getgu(){
        return this.gu;
    }
    public GPS getgl(){
        return this.gl;
    }
    public String getcde(){
        return this.cde;
    }
    
}


/**
 * Escreva a descrição da classe GestaoTotal aqui.
 * 
 * @author (seu nome)
 * @version (número de versão ou data)
 */

import java.io.*;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class GestaoTotal implements Serializable {

    private GestaoEncomendas ge;
    private GestaoVoluntarios gv;
    private GestaoEmpresas gem;
    private GestaoLojas gl;
    private GestaoUtilizadores gu;

    // construtor vazio
    public GestaoTotal() {
        this.ge = new GestaoEncomendas();
        this.gv = new GestaoVoluntarios();
        this.gem = new GestaoEmpresas();
        this.gl = new GestaoLojas();
        this.gu = new GestaoUtilizadores();
    }

    // construtor
    public GestaoTotal(GestaoEncomendas ge, GestaoVoluntarios gv, GestaoEmpresas gem, GestaoLojas gl,
            GestaoUtilizadores gu) {
        this.ge = ge;
        this.gv = gv;
        this.gem = gem;
        this.gl = gl;
        this.gu = gu;
    }

    // construtor
    public GestaoTotal(GestaoTotal gt) {
        this.ge = gt.getGE();
        this.gv = gt.getGV();
        this.gem = gt.getGEMP();
        this.gl = gt.getGL();
        this.gu = gt.getGU();
    }

    /**
     * 
     * Encomendas
     */
    public GestaoEncomendas getGE() {
        return ge;
    }

    public void adicionaEncomenda(Encomenda novoEncomenda) {
        ge.addEncomenda(novoEncomenda);
    }

    public void adicionaEncomendaAceite(String codEnc, String codEntrega) {
        ge.getEncAceites().put(codEnc, codEntrega);
    }

    public void removeEncomenda(String codEnc) {
        ge.getEncomendas().remove(codEnc);
    }

    public boolean existeEncomenda(String refEncomenda) {
        return this.ge.getEncomendas().containsKey(refEncomenda);
    }

    /**
     * public boolean existeProdutoEncomenda(String refProduto){ int i; int j;
     * for(j=0;j<ge.getEncomendas().values().size();j++){
     * for(i=0;i<ge.getEncomendas().values().getLinhas().size();i++){
     * if(linhas.get(i).getCodProduto()==refProduto) return true; } } return false;
     * }
     * 
     */
    /**
     * 
     * Voluntarios
     */
    public GestaoVoluntarios getGV() {
        return gv;
    }

    public void adicionaVoluntario(Voluntario novoVoluntario) {
        gv.getVoluntarios().put(novoVoluntario.getCodV(), novoVoluntario);
    }

    public void removeVoluntarios(String codV) {
        gv.getVoluntarios().remove(codV);
    }

    /**
     * 
     * Empresas
     */

    public GestaoEmpresas getGEMP() {
        return gem;
    }

    public void adicionaEmpresa(EmpresaTransportadora novaEmpresa) {
        gem.getEmpresasTransp().put(novaEmpresa.getCodEmp(), novaEmpresa);
    }

    public void removeEmpresa(String codEmp) {
        gem.getEmpresasTransp().remove(codEmp);
    }

    /**
     * 
     * Lojas
     */
    public GestaoLojas getGL() {
        return gl;
    }

    public void adicionaLoja(Loja novaLoja) {
        gl.getLojas().put(novaLoja.getCodLoja(), novaLoja);
    }

    public void removeLoja(String codLoja) {
        gl.getLojas().remove(codLoja);
    }

    /**
     * Utilizadores
     */
    public GestaoUtilizadores getGU() {
        return gu;
    }

    public void adicionaUtilizador(Utilizador novoUtilizador) {
        gu.getUtilizadores().put(novoUtilizador.getCodU(), novoUtilizador);
    }

    public void removeUtilizador(String codU) {
        gu.getUtilizadores().remove(codU);
    }

    public boolean existeUtilizador(String user) {
        // String nova = user.replace("@email.com", "");
        return this.gu.getUtilizadores().containsKey(user);
    }

    public boolean existeCodigoLoja(String cod) {
        return this.gl.getLojas().containsKey(cod);
    }

    public boolean existeVoluntario(String cod) {
        return this.gv.getVoluntarios().containsKey(cod);
    }

    public boolean existeLoja(String cod) {
        return this.gl.getLojas().containsKey(cod);
    }

    public boolean existeEmpresa(String codEmp) {
        return this.gem.getEmpresasTransp().containsKey(codEmp);
    }

    public void setGEMP(GestaoEmpresas gem2) {
        this.gem = gem2;
    }

    public void setGT(GestaoEncomendas ge2) {
        this.ge = ge2;
    }

    public void setGL(GestaoLojas gl2) {
        this.gl = gl2;
    }

    public void setGU(GestaoUtilizadores gu2) {
        this.gu = gu2;
    }

    public void setGV(GestaoVoluntarios gv2) {
        this.gv = gv2;
    }

    // Existe email utilizador
    public boolean existeEmail(String user) {
        for (Entry<String, Utilizador> map : this.gu.getUtilizadores().entrySet()) {
            String k = map.getKey();
            if(this.gu.getUtilizadores().get(k).getUser().equals(user)) return true;
        }
        return false;

    }
    
    //Existe loja com o mesmo nome
    public boolean existeNomeLoja(String loja) {
        for (Entry<String, Loja> map : this.gl.getLojas().entrySet()) {
            String k = map.getKey();
            if(this.gl.getLojas().get(k).getNomeLoja().equals(loja)) return true;
        }
        return false;

    }
    
    //Existe email loja com o mesmo nome
    public boolean existeEmailLoja(String email) {
        for (Entry<String, Loja> map : this.gl.getLojas().entrySet()) {
            String k = map.getKey();
            if(this.gl.getLojas().get(k).getUser().equals(email)) return true;
        }
        return false;

    }
    
    //Existe empresa com o mesmo nome
    public boolean existeNomeEmpresa(String nome) {
        for (Entry<String, EmpresaTransportadora> map : this.gem.getEmpresasTransp().entrySet()) {
            String k = map.getKey();
            if(this.gem.getEmpresasTransp().get(k).getNomeEmpresa().equals(nome)) return true;
        }
        return false;

    }
    
    //Existe empresa com o mesmo email
    public boolean existeEmailEmpresa(String nome) {
        for (Entry<String, EmpresaTransportadora> map : this.gem.getEmpresasTransp().entrySet()) {
            String k = map.getKey();
            if(this.gem.getEmpresasTransp().get(k).getUser().equals(nome)) return true;
        }
        return false;

    }
    
    //Existe nome voluntario 
    public boolean existeNomeVoluntario(String nome) {
        for (Entry<String, Voluntario> map : this.gv.getVoluntarios().entrySet()) {
            String k = map.getKey();
            if(this.gv.getVoluntarios().get(k).getNomeVoluntario().equals(nome)) return true;
        }
        return false;

    }
    
    //Existe email voluntario
    public boolean existeEmailVoluntario(String nome) {
        for (Entry<String, Voluntario> map : this.gv.getVoluntarios().entrySet()) {
            String k = map.getKey();
            if(this.gv.getVoluntarios().get(k).getUser().equals(nome)) return true;
        }
        return false;
    }

	public void alteraEstadoEncomenda(String nr, String distribuidor) {
        this.ge.mudarEstadoEncomenda(nr, distribuidor);
	}

	public void printEncomendasPendentes(GestaoTotal gt, String codVol) {
        this.getGE().printEncPendentes(gt,codVol);
    }
    // custo de transporte entre utilizador e empresa
    public double custoTransporteEmp(String codU, String codEmp){
        try{
            // verificar se existe a empresa
            gem.getEmpresasTransp().containsKey(codEmp);
            // verificar se existe o utilizador
            gu.getUtilizadores().containsKey(codU);
            // depois de saber que existe, calcula a distancia
            return  (
                    //custo de transporte +
                    gem.getEmpresasTransp().get(codEmp).getCustoTransporte() +
                    (
                    // taxa * distancia
                    gem.getEmpresasTransp().get(codEmp).getTaxa()
                     *
                    distancia(gem.getEmpresasTransp().get(codEmp).getLocalizacao(), 
                              gu.getUtilizadores().get(codU).getLocalizacao())
                    )
                    // * ida e volta
                    * 2
                    );
        }
        catch(NullPointerException e){
            System.out.println("Não existe o código que indicou.");
        }
        return 0.0;
    }

    public double distancia(Localizacao a, Localizacao b){
        return Math.sqrt(Math.pow(a.getX() - b.getX(), 2) + Math.pow(a.getY() - b.getY(),2));
    }

    // dado um codigo (voluntario ou empresa), calcula a faturacao
    public double faturacaoVOLeEMP(String codigo){
        double res = 0;
        // todas as keys do map de fora
        for (Map.Entry<String, Map<String, List<Encomenda>>> mapFora : this.ge.getEncomendas().entrySet()) {
            // todas as keys do map de dentro
            for (Map.Entry<String, List<Encomenda>> mapDentro : mapFora.getValue().entrySet()) {
                // 3) Todas as encomendas da lista
                for(Encomenda e : mapDentro.getValue()){
                    // verifica quem entregou a encomenda
                    if(e.getdistribuidor()!=null && e.getdistribuidor().equals(codigo)){
                        // se é o distribuidor que queremos, adiciona ao resultado
                        res += this.custoTransporteEmp(e.getCodU(), codigo);
                    }
                    // caso contrário passa à frente
                }
            }
        }
        return res;
    }

}

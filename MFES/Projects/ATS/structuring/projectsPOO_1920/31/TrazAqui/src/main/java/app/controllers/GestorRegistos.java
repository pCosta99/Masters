package app.controllers;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import app.enums.EstadosEncomendaEnum;
import app.enums.EstadosTransportadorEnum;
import app.enums.TiposUtilizadoresEnum;
import app.exceptions.CodigoProdutoJaExistenteException;
import app.exceptions.CodigoProdutoNaoExistenteException;
import app.exceptions.EmpresaTransportadoraNaoExistenteException;
import app.exceptions.EncomendaJaCanceladaException;
import app.exceptions.EncomendaNaoExistenteException;
import app.exceptions.EstadoRegressivoException;
import app.exceptions.LojaNaoExistenteException;
import app.exceptions.PasswordErradaException;
import app.exceptions.SemPermissaoAprovarTransporteException;
import app.exceptions.SemPermissaoClassificarEntregaException;
import app.exceptions.SemPermissaoEfectuarAgendamentoEnvioException;
import app.exceptions.SemPermissaoEfectuarEncomendaException;
import app.exceptions.SemPermissaoEstadoTransportadorException;
import app.exceptions.SemPermissaoTransporteEncException;
import app.exceptions.SemPermissaoTransporteEncMedicaException;
import app.exceptions.SemPermissaoVerificarFaturacaoException;
import app.exceptions.TransportadorOcupadoException;
import app.exceptions.UtilizadorJaExistenteException;
import app.exceptions.UtilizadorNaoExistenteException;
import app.exceptions.VoluntarioAindaAEfectuarTransporteException;
import app.interfaces.IInfo;
import app.models.EmpresaTransportadora;
import app.models.Encomenda;
import app.models.EncomendaMedica;
import app.models.Info;
import app.models.Localizacao;
import app.models.Loja;
import app.models.Produto;
import app.models.RegistoEncomenda;
import app.models.Transportador;
import app.models.Utilizador;
import app.models.Voluntario;

public class GestorRegistos implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = -5986411141916568833L;
    // #region variables
    private IInfo utilizadorActual;
    private RegistosUtilizador utilizadores;
    private RegistosEncomendas encomendas;
    private RegistosProdutos produtos;
    private static String errorMessageSemPermissaoEfetuarEncomenda =
            "Sem Permissão Para Efetuar Encomenda !!\n";
    private static String errorMessageSemPermissaoEfectuarAgendamentoEnvio =
            "Sem Permissao Efectuar Agendamento Envio !!\n";
    private static String errorMessageSemPermissaoTransporteEncMedica =
            "Sem Permissao Efectuar Transporte de Encomenda Médica !!\n";
    private static String errorMessageSemPermissaoTransporteEnc =
            "Sem Permissao Efectuar Transporte de Encomenda !!\n";
    private static String errorMessageTransportadorOcupado =
            "Transportador Ocupada de Momento !!\n";
    private static String errorMessageEmpresaTransportadoraNaoExistente =
            "Empresa Transportadora Não Existente !!\n";
    private static String errorMessageLojaNaoExistente = "Loja Não Existente !!\n";
    private static String errorMessageSemPermissaoAprovarTransporte =
            "Sem Permissao Aprovar Transporte de Encomenda !!\n";
    private static String errorMessageSemPermissaoClassificarEntrega =
            "Sem Permissao Classificar Entrega !!\n";
    private static String errorMessageSemPermissaoVerificarFaturacao =
            "Sem Permissao Verificar Faturacao !!\n";
    private static String errorMessageSemPermissaoEstadoTransportador =
            "Sem Permissao Sinalizar Estado !!\n";

    // #endregion

    // #region Construtores

    public GestorRegistos() {
        this.utilizadorActual = new Info();
        this.utilizadores = new RegistosUtilizador();
        this.encomendas = new RegistosEncomendas();
        this.produtos = new RegistosProdutos();
    }


    /**
     * @param utilizadorActual
     * @param utilizadores
     * @param encomendas
     */
    public GestorRegistos(IInfo utilizadorActual, RegistosUtilizador utilizadores,
            RegistosEncomendas encomendas, RegistosProdutos produtos) {
        this.setEncomendas(encomendas);
        this.setUtilizadorActual(utilizadorActual);
        this.setUtilizadores(utilizadores);
        this.setProdutos(produtos);
    }

    /**
     * @param g
     */
    public GestorRegistos(GestorRegistos g) {
        this.setEncomendas(g.encomendas);
        this.setUtilizadorActual(g.utilizadorActual);
        this.setUtilizadores(g.utilizadores);
        this.setProdutos(g.produtos);
    }
    // #endregion

    // #region getters setter
    /**
     * @return the utilizadorActual
     */
    public IInfo getUtilizadorActual() {
        return (this.utilizadorActual == null) ? null : this.utilizadorActual.clone();
    }

    /**
     * @return the encomendas
     */
    public RegistosEncomendas getEncomendas() {
        return this.encomendas.clone();
    }

    /**
     * @param encomendas the encomendas to set
     */
    public void setEncomendas(RegistosEncomendas encomendas) {
        this.encomendas = encomendas.clone();
    }

    /**
     * @return the utilizadores
     */
    public RegistosUtilizador getUtilizadores() {
        return this.utilizadores.clone();
    }

    /**
     * @param utilizadores the utilizadores to set
     */
    public void setUtilizadores(RegistosUtilizador utilizadores) {
        this.utilizadores = utilizadores.clone();
    }

    /**
     * @param utilizadorActual the utilizadorActual to set
     */
    public void setUtilizadorActual(IInfo utilizadorActual) {
        this.utilizadorActual = (utilizadorActual == null) ? null : utilizadorActual.clone();
    }


    /**
     * @return the produtos
     */
    public RegistosProdutos getProdutos() {
        return this.produtos.clone();
    }

    /**
     * @param produtos the produtos to set
     */
    public void setProdutos(RegistosProdutos produtos) {
        this.produtos = produtos.clone();
    }
    // #endregion

    // #region Overrrides
    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        GestorRegistos gestor = (GestorRegistos) o;
        // field comparison
        return Objects.equals(this.utilizadorActual, gestor.utilizadorActual)
                && Objects.equals(this.utilizadores, gestor.utilizadores)
                && Objects.equals(this.produtos, gestor.produtos)
                && Objects.equals(this.encomendas, gestor.encomendas);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador Actual: ");
        sb.append(this.utilizadorActual.toString());
        sb.append('\n');
        sb.append("Utilizadores: ");
        sb.append(this.utilizadores.toString());
        sb.append('\n');
        sb.append("Produtos: ");
        sb.append(this.produtos.toString());
        sb.append('\n');
        sb.append("Encomendas: ");
        sb.append(this.encomendas.toString());
        sb.append('\n');
        return sb.toString();
    }

    public GestorRegistos clone() {
        return new GestorRegistos(this);
    }

    // #endregion


    // #region Methods

    public void adicionaProduto(String codLoja, Produto p)
            throws CodigoProdutoJaExistenteException {
        this.produtos.adicionaProduto(codLoja, p);
    }

    public boolean existeProduto(String codLoja, String codProduto) {
        return this.produtos.existeProduto(codLoja, codProduto);
    }

    public Produto devolveProduto(String codLoja, String codProduto)
            throws CodigoProdutoNaoExistenteException {
        return this.produtos.devolveProduto(codLoja, codProduto);
    }

    public List<Produto> listaProdutos(String codLoja) {
        return this.produtos.listaProdutos(codLoja);
    }

    public void removeProduto(String codLoja, String codProduto)
            throws CodigoProdutoNaoExistenteException {
        this.produtos.removeProduto(codLoja, codProduto);
    }

    public void removeProduto(String codLoja, Produto p) throws CodigoProdutoNaoExistenteException {
        this.produtos.removeProduto(codLoja, p);
    }

    public void atualizaProduto(String codLoja, Produto prod) {
        this.produtos.atualizaProduto(codLoja, prod);
    }

    public void registaNovoTipoUtilizador(IInfo i) throws UtilizadorJaExistenteException {
        this.utilizadores.adicionaUtilizador(i);
    }

    public boolean existeUtilizador(String email) {
        return this.utilizadores.existeUtilizador(email);
    }

    public IInfo getUtilizador(String email) throws UtilizadorNaoExistenteException {
        return this.utilizadores.getUtilizador(email);
    }

    public void login(String email, String password)
            throws UtilizadorNaoExistenteException, PasswordErradaException {
        IInfo utilizador = this.utilizadores.getUtilizador(email);
        if (utilizador.passOK(password)) {
            this.setUtilizadorActual(utilizador);
        }
    }

    public void logout() {
        this.utilizadorActual = null;
    }


    public void inserirEncomendaParser(Encomenda e) {
        RegistoEncomenda re =
                new RegistoEncomenda(e, EstadosEncomendaEnum.ABERTA, null, -1, null, null, null, 0);
        this.encomendas.adicionaRegistoEncomenda(re);

    }

    public void inserirPedidoLoja(String codLoja, Encomenda e)
            throws SemPermissaoEfectuarEncomendaException {
        if (this.utilizadorActual instanceof Utilizador) {
            e.setCodEnc(this.getNovoCodigoEnc());
            e.setCodLoja(codLoja);
            e.setCodUtilizador(this.utilizadorActual.getEmail());
            RegistoEncomenda re = new RegistoEncomenda(e, EstadosEncomendaEnum.ABERTA, null, -1,
                    null, null, null, 0);
            this.encomendas.adicionaRegistoEncomenda(re);
        } else {
            throw new SemPermissaoEfectuarEncomendaException(
                    errorMessageSemPermissaoEfetuarEncomenda);
        }
    }

    private String getNovoCodigoEnc() {
        String cod = "";
        boolean done = false;
        int count = this.encomendas.getEncomendas().size();
        while (!done) {
            cod = "e" + count;
            if (this.encomendas.existeEncomenda(cod)) {
                count++;
            } else {
                done = true;
            }
        }
        return cod;
    }

    public boolean existeEncomenda(String codEnc) {
        return this.encomendas.existeEncomenda(codEnc);
    }

    public RegistoEncomenda getRegistoEncomenda(String codEnc)
            throws EncomendaNaoExistenteException {
        return this.encomendas.getRegistoEncomenda(codEnc);
    }

    public List<String> listaTransportadoresAguardaAprovacaoEnc(String codEnc)
            throws EncomendaNaoExistenteException {
        return this.encomendas.getListaTransportadoresEnc(codEnc);
    }

    public void acrescentaFilaEspera(int qnt)
            throws LojaNaoExistenteException, UtilizadorNaoExistenteException {
        if (this.utilizadorActual instanceof Loja) {
            Loja l = (Loja) this.utilizadorActual;
            l.setFilaEspera(qnt);
            this.utilizadores.atualizaUtilizador(l.getEmail(), l);
        } else {
            throw new LojaNaoExistenteException("Utilizador não é do tipo Loja!");
        }
    }

    public void encomendAceiteParser(String codEnc) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {
        this.encomendas.sinalizaEncomendaPronta(codEnc);
    }

    public void encomendaProntaEntrega(String codEnc)
            throws SemPermissaoEfectuarAgendamentoEnvioException, EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {
        if (this.utilizadorActual instanceof Loja) {
            RegistoEncomenda re = this.encomendas.getRegistoEncomenda(codEnc);
            if (!re.getEncomenda().getCodLoja().equals(this.utilizadorActual.getEmail())) {
                throw new SemPermissaoEfectuarAgendamentoEnvioException(
                        errorMessageSemPermissaoEfectuarAgendamentoEnvio);
            }
            this.encomendas.sinalizaEncomendaPronta(codEnc);
        } else {
            throw new SemPermissaoEfectuarAgendamentoEnvioException(
                    errorMessageSemPermissaoEfectuarAgendamentoEnvio);
        }
    }

    public void indicaEncomendaParaTransporte(String codEnc)
            throws SemPermissaoTransporteEncException, SemPermissaoTransporteEncMedicaException,
            EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException, TransportadorOcupadoException {
        if (this.utilizadorActual instanceof Transportador) {
            Transportador transportador = (Transportador) this.utilizadorActual;
            if (this.encomendas.getRegistoEncomenda(codEnc)
                    .getEncomenda() instanceof EncomendaMedica
                    && !transportador.aceitoTransporteMedicamentos()) {
                throw new SemPermissaoTransporteEncMedicaException(
                        errorMessageSemPermissaoTransporteEncMedica);
            }
            if (transportador.getEstado() == EstadosTransportadorEnum.OCUPADO) {
                throw new TransportadorOcupadoException(errorMessageTransportadorOcupado);
            }
            if (this.utilizadorActual instanceof Voluntario) {
                this.encomendas.sinalizaEncomendaTransporteEnvio(codEnc,
                        this.utilizadorActual.getEmail());
            } else if (this.utilizadorActual instanceof EmpresaTransportadora) {
                this.encomendas.sinalizaEncomendaTransporteDisponivel(codEnc,
                        this.utilizadorActual.getEmail());
            }
        } else {
            throw new SemPermissaoTransporteEncException(errorMessageSemPermissaoTransporteEnc);
        }
    }

    // Transporte de Encomenda
    public boolean transportaEncomenda(String codEnc) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException,
            SemPermissaoTransporteEncMedicaException, TransportadorOcupadoException,
            SemPermissaoTransporteEncException, SemPermissaoEstadoTransportadorException,
            UtilizadorNaoExistenteException, VoluntarioAindaAEfectuarTransporteException {
        boolean ocupado = false;
        if (this.utilizadorActual instanceof Transportador) {
            Transportador transportador = (Transportador) this.utilizadorActual;
            if (this.encomendas.getRegistoEncomenda(codEnc)
                    .getEncomenda() instanceof EncomendaMedica
                    && !transportador.aceitoTransporteMedicamentos()) {
                throw new SemPermissaoTransporteEncMedicaException(
                        errorMessageSemPermissaoTransporteEncMedica);
            }
            if (transportador.getEstado() == EstadosTransportadorEnum.OCUPADO) {
                throw new TransportadorOcupadoException(errorMessageTransportadorOcupado);
            }
            if (this.utilizadorActual instanceof Voluntario) {
                this.encomendas.sinalizaEnvio(codEnc);
                this.alteraEstadoTransp(EstadosTransportadorEnum.OCUPADO);
                ocupado = true;
            } else if (this.utilizadorActual instanceof EmpresaTransportadora) {
                this.encomendas.sinalizaEnvio(codEnc);
            }
        } else {
            throw new SemPermissaoTransporteEncException(errorMessageSemPermissaoTransporteEnc);
        }
        return ocupado;
    }

    // Transporte de Encomenda
    public boolean encomendaEntregue(String codEnc)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException, SemPermissaoTransporteEncMedicaException,
            SemPermissaoTransporteEncException, SemPermissaoEstadoTransportadorException,
            UtilizadorNaoExistenteException, VoluntarioAindaAEfectuarTransporteException {
        boolean ocupado = true;
        if (this.utilizadorActual instanceof Transportador) {
            Transportador transportador = (Transportador) this.utilizadorActual;
            if (this.encomendas.getRegistoEncomenda(codEnc)
                    .getEncomenda() instanceof EncomendaMedica
                    && !transportador.aceitoTransporteMedicamentos()) {
                throw new SemPermissaoTransporteEncMedicaException(
                        errorMessageSemPermissaoTransporteEncMedica);
            }
            if (this.utilizadorActual instanceof Voluntario) {
                this.encomendas.sinalizaRececao(codEnc);
                this.alteraEstadoTransp(EstadosTransportadorEnum.LIVRE);
                ocupado = false;
            } else if (this.utilizadorActual instanceof EmpresaTransportadora) {
                this.encomendas.sinalizaRececao(codEnc);
            }
        } else {
            throw new SemPermissaoTransporteEncException(errorMessageSemPermissaoTransporteEnc);
        }
        return ocupado;
    }

    public void alteraEstadoTransp(EstadosTransportadorEnum estado)
            throws SemPermissaoEstadoTransportadorException, UtilizadorNaoExistenteException,
            VoluntarioAindaAEfectuarTransporteException {
        if (this.utilizadorActual instanceof Transportador) {
            Transportador transportador = (Transportador) this.utilizadorActual;
            if (transportador.getEstado() != estado) {
                if (transportador instanceof Voluntario) {
                    if ((transportador.getEstado() == EstadosTransportadorEnum.LIVRE)
                            || (transportador.getEstado() == EstadosTransportadorEnum.OCUPADO
                                    && this.listagemPorUtilizadorPorDataDentroRaioAccao(
                                            EstadosEncomendaEnum.ENVIADA).isEmpty())) {
                        transportador.setEstado(estado);
                        this.utilizadores.atualizaUtilizador(transportador.getEmail(),
                                transportador);
                    } else {
                        throw new VoluntarioAindaAEfectuarTransporteException();
                    }
                } else {
                    transportador.setEstado(estado);
                    this.utilizadores.atualizaUtilizador(transportador.getEmail(), transportador);
                }
            }
        } else {
            throw new SemPermissaoEstadoTransportadorException(
                    errorMessageSemPermissaoEstadoTransportador);
        }
    }

    public void aprovaPreco(String codEnc, String codTransportador)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException, SemPermissaoAprovarTransporteException,
            UtilizadorNaoExistenteException, LojaNaoExistenteException,
            EmpresaTransportadoraNaoExistenteException {
        if (this.utilizadorActual instanceof Utilizador) {
            RegistoEncomenda re = this.encomendas.getRegistoEncomenda(codEnc);
            if (!re.getEncomenda().getCodUtilizador().equals(this.utilizadorActual.getEmail())) {
                throw new SemPermissaoAprovarTransporteException();
            }
            re.setPreco(this.getPrecoTransporte(codEnc, codTransportador));
            this.encomendas.atualizaRegisto(re);
            this.encomendas.sinalizaEncomendaTransporteEnvio(codEnc, codTransportador);
        } else {
            throw new SemPermissaoAprovarTransporteException(
                    errorMessageSemPermissaoAprovarTransporte);
        }

    }

    public double getPrecoTransporte(String codEnc, String codTrans)
            throws UtilizadorNaoExistenteException, EncomendaNaoExistenteException,
            LojaNaoExistenteException, EmpresaTransportadoraNaoExistenteException {
        RegistoEncomenda enc = this.encomendas.getRegistoEncomenda(codEnc);
        Loja loja;
        IInfo transp;

        try {
            loja = (Loja) this.utilizadores.getUtilizador(enc.getEncomenda().getCodLoja());

        } catch (UtilizadorNaoExistenteException e) {
            throw new LojaNaoExistenteException(errorMessageLojaNaoExistente);
        }

        IInfo cliente = this.utilizadores.getUtilizador(enc.getEncomenda().getCodUtilizador());

        try {
            transp = this.utilizadores.getUtilizador(codTrans);

        } catch (UtilizadorNaoExistenteException e) {
            throw new EmpresaTransportadoraNaoExistenteException(
                    errorMessageEmpresaTransportadoraNaoExistente);
        }

        EmpresaTransportadora empresa = (EmpresaTransportadora) transp;

        return empresa.precoTransporte(loja.getLocalizacao(), cliente.getLocalizacao(),
                enc.getEncomenda().getPeso(), loja.getFilaEspera(), 1);
    }

    public void classificaEntrega(String codEnc, int classificacao)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException, SemPermissaoClassificarEntregaException {
        if (this.utilizadorActual instanceof Utilizador) {
            RegistoEncomenda re = this.encomendas.getRegistoEncomenda(codEnc);
            if (!re.getEncomenda().getCodUtilizador().equals(this.utilizadorActual.getEmail())) {
                throw new SemPermissaoClassificarEntregaException(
                        errorMessageSemPermissaoClassificarEntrega);
            }
            this.encomendas.classificaEntrega(codEnc, classificacao);
        } else {
            throw new SemPermissaoClassificarEntregaException(
                    errorMessageSemPermissaoClassificarEntrega);
        }
    }

    public Set<RegistoEncomenda> listagemPorUtilizadorPorData(EstadosEncomendaEnum estado) {
        return listagemPorUtilizadorPorData().stream()
                .filter(re -> re.getEstadoEncomenda() == estado)
                .collect(Collectors.toCollection(() -> new TreeSet<>(
                        Comparator.comparing(re -> re.getEncomenda().getDataCriacao()))));
    }

    public Set<RegistoEncomenda> listagemPorUtilizadorPorData() {
        Set<RegistoEncomenda> lista = new TreeSet<>();
        if (this.utilizadorActual instanceof Transportador) {
            lista = this.encomendas.getEncomendas().keySet().stream()
                    .filter(e -> e.getCodTransportador() != null
                            && e.getCodTransportador().equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone)
                    .collect(Collectors.toCollection(() -> new TreeSet<>(
                            Comparator.comparing(re -> re.getEncomenda().getDataCriacao()))));
        } else if (this.utilizadorActual instanceof Loja) {
            lista = this.encomendas.getEncomendas().keySet().stream()
                    .filter(e -> e.getEncomenda().getCodLoja()
                            .equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone)
                    .collect(Collectors.toCollection(() -> new TreeSet<>(
                            Comparator.comparing(re -> re.getEncomenda().getDataCriacao()))));
        } else if (this.utilizadorActual instanceof Utilizador) {
            lista = this.encomendas.getEncomendas().keySet().stream()
                    .filter(e -> e.getEncomenda().getCodUtilizador()
                            .equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone)
                    .collect(Collectors.toCollection(() -> new TreeSet<>(
                            Comparator.comparing(re -> re.getEncomenda().getDataCriacao()))));
        }

        return lista;
    }

    public Set<RegistoEncomenda> listagemPorUtilizadorPorDataDentroRaioAccao()
            throws UtilizadorNaoExistenteException {
        Set<RegistoEncomenda> lista =
                new TreeSet<>(Comparator.comparing(re -> re.getEncomenda().getDataCriacao()));
        if (this.utilizadorActual instanceof Transportador) {
            Transportador transportador = (Transportador) this.utilizadorActual;
            for (RegistoEncomenda registoEncomenda : this.encomendas.getEncomendas().keySet()) {
                Utilizador u = (Utilizador) this.utilizadores
                        .getUtilizador(registoEncomenda.getEncomenda().getCodUtilizador());
                Loja l = (Loja) this.utilizadores
                        .getUtilizador(registoEncomenda.getEncomenda().getCodLoja());
                if (transportador.dentroRaioAccao(u.getLocalizacao())
                        && transportador.dentroRaioAccao(l.getLocalizacao())) {
                    lista.add(registoEncomenda.clone());
                }
            }
        }
        return lista;
    }

    public Set<RegistoEncomenda> listagemPorUtilizadorPorDataDentroRaioAccao(
            EstadosEncomendaEnum estado) throws UtilizadorNaoExistenteException {
        return listagemPorUtilizadorPorDataDentroRaioAccao().stream()
                .filter(re -> re.getEstadoEncomenda() == estado)
                .collect(Collectors.toCollection(() -> new TreeSet<>(
                        Comparator.comparing(re -> re.getEncomenda().getDataCriacao()))));
    }

    public List<RegistoEncomenda> listagemEncomendasAguardarAprovacao()
            throws SemPermissaoAprovarTransporteException {
        if (!(this.utilizadorActual instanceof Utilizador)) {
            throw new SemPermissaoAprovarTransporteException(
                    errorMessageSemPermissaoAprovarTransporte);
        }
        return this.encomendas.getListRegistoPorEstado(this.utilizadorActual.getEmail(),
                EstadosEncomendaEnum.AGUARDAAPROVACAO);
    }

    public List<RegistoEncomenda> listagemEncomendasAguardarClassificacao()
            throws SemPermissaoClassificarEntregaException {
        if (!(this.utilizadorActual instanceof Utilizador)) {
            throw new SemPermissaoClassificarEntregaException(
                    errorMessageSemPermissaoClassificarEntrega);
        }
        return this.encomendas
                .getListRegistoPorEstado(this.utilizadorActual.getEmail(),
                        EstadosEncomendaEnum.RECEBIDA)
                .stream().filter(re -> re.getClassificacao() == -1).collect(Collectors.toList());
    }

    public Set<RegistoEncomenda> listagemPorUtilizador() {
        Set<RegistoEncomenda> lista = new TreeSet<>();
        if (this.utilizadorActual instanceof Voluntario) {
            lista = this.encomendas.getEncomendas().keySet().stream()
                    .filter(e -> e.getCodTransportador() != null
                            && e.getCodTransportador().equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone).collect(Collectors.toCollection(TreeSet::new));
        } else if (this.utilizadorActual instanceof EmpresaTransportadora) {
            lista = this.encomendas.getEncomendas().entrySet().stream()
                    .filter(e -> (e.getKey().getCodTransportador() != null && e.getKey()
                            .getCodTransportador().equals(this.utilizadorActual.getEmail()))
                            || e.getValue().contains(this.utilizadorActual.getEmail()))
                    .map(e -> e.getKey().clone()).collect(Collectors.toCollection(TreeSet::new));
        } else if (this.utilizadorActual instanceof Loja) {
            lista = this.encomendas.getEncomendas().keySet().stream().filter(
                    e -> e.getEncomenda().getCodLoja().equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone).collect(Collectors.toCollection(TreeSet::new));
        } else if (this.utilizadorActual instanceof Utilizador) {
            lista = this.encomendas.getEncomendas().keySet().stream()
                    .filter(e -> e.getEncomenda().getCodUtilizador()
                            .equals(this.utilizadorActual.getEmail()))
                    .map(RegistoEncomenda::clone).collect(Collectors.toCollection(TreeSet::new));
        }

        return lista;
    }

    public double totalFaturadoPeriodo(LocalDateTime inicio, LocalDateTime fim)
            throws SemPermissaoVerificarFaturacaoException {
        if (!(this.utilizadorActual instanceof EmpresaTransportadora)) {
            throw new SemPermissaoVerificarFaturacaoException(
                    errorMessageSemPermissaoVerificarFaturacao);
        }
        return this.encomendas
                .getListRegistoUtilizadorEspacoTempo(this.utilizadorActual.getEmail(), inicio, fim)
                .stream().mapToDouble(RegistoEncomenda::getPreco).sum();
    }

    public List<String> listagemDezUtilizadoresMaisEnc() {

        Map<Long, List<String>> count = new TreeMap<>(Comparator.reverseOrder());
        for (IInfo utilizador : this.utilizadores.getListaRegistos().values()) {
            if (utilizador instanceof Utilizador) {
                long qnt = contaEncomendasUtilizadorTransportadas(utilizador.getEmail());
                if (count.containsKey(qnt)) {
                    count.get(qnt).add(utilizador.getEmail() + " " + qnt + " encomendas");
                } else {
                    List<String> actual = new ArrayList<>();
                    actual.add(utilizador.getEmail() + " " + qnt + " encomendas");
                    count.put(qnt, actual);
                }
            }
        }
        List<String> listaFinal = new ArrayList<>();
        Iterator<Long> it = count.keySet().iterator();
        while (it.hasNext() && listaFinal.size() < 10) {
            List<String> lts = count.get(it.next());
            for (String string : lts) {
                if (listaFinal.size() < 10) {
                    listaFinal.add(string);
                }
            }
        }
        return listaFinal;

    }

    private long contaEncomendasUtilizadorTransportadas(String codUtilizador) {
        return this.encomendas.getEncomendas().keySet().stream()
                .filter(re -> re.getEncomenda().getCodUtilizador().equals(codUtilizador)
                        && re.getEstadoEncomenda() == EstadosEncomendaEnum.RECEBIDA)
                .count();
    }

    public List<String> listagemDezEmpresasMaisKm() {

        Map<Double, List<String>> count = new TreeMap<>(Comparator.reverseOrder());
        for (IInfo empresa : this.utilizadores.getListaRegistos().values()) {
            if (empresa instanceof EmpresaTransportadora) {
                double qnt = contaTotalKmEmpresaTransportador(empresa.getEmail());
                if (count.containsKey(qnt)) {
                    count.get(qnt).add(empresa.getEmail() + " " + qnt + " km");
                } else {
                    List<String> actual = new ArrayList<>();
                    actual.add(empresa.getEmail() + " " + qnt + " km");
                    count.put(qnt, actual);
                }
            }
        }
        List<String> listaFinal = new ArrayList<>();
        Iterator<Double> it = count.keySet().iterator();
        while (it.hasNext() && listaFinal.size() < 10) {
            List<String> lts = count.get(it.next());
            for (String string : lts) {
                if (listaFinal.size() < 10) {
                    listaFinal.add(string);
                }
            }
        }
        return listaFinal;

    }

    private double contaTotalKmEmpresaTransportador(String codUtilizador) {
        Localizacao locTransP =
                this.getUtilizadores().getListaRegistos().get(codUtilizador).getLocalizacao();
        List<RegistoEncomenda> lista = this.encomendas.getEncomendas().keySet().stream()
                .filter(re -> re.getCodTransportador() != null
                        && re.getCodTransportador().equals(codUtilizador)
                        && re.getEstadoEncomenda() == EstadosEncomendaEnum.RECEBIDA)
                .map(RegistoEncomenda::clone).collect(Collectors.toList());

        double res = 0;
        for (RegistoEncomenda registoEncomenda : lista) {
            Localizacao locLoja = this.getUtilizadores().getListaRegistos()
                    .get(registoEncomenda.getEncomenda().getCodLoja()).getLocalizacao();
            Localizacao locCliente = this.getUtilizadores().getListaRegistos()
                    .get(registoEncomenda.getEncomenda().getCodUtilizador()).getLocalizacao();

            res += (locTransP.distancia(locLoja) + locLoja.distancia(locCliente));
        }

        return res;
    }

    public List<IInfo> listagemUtilizadoresTipo(TiposUtilizadoresEnum t) {
        return this.utilizadores.listaUtilizadores(t);
    }

    public List<IInfo> listagemTransportadoresIntervaloTempo(LocalDateTime dataInicio,
            LocalDateTime dataFim) {
        List<String> lstCodTransp = this.encomendas
                .getListRegistoUtilizadorEspacoTempo(this.utilizadorActual.getEmail(), dataInicio,
                        dataFim)
                .stream().map(RegistoEncomenda::getCodTransportador).distinct()
                .collect(Collectors.toList());
        List<IInfo> lstTransp = new ArrayList<>();
        for (String cod : lstCodTransp) {
            try {
                lstTransp.add(this.utilizadores.getUtilizador(cod));
            } catch (UtilizadorNaoExistenteException e) {
            }
        }
        return lstTransp;
    }


    public List<RegistoEncomenda> listagemEncTranspPorEmIntervaloTempo(String codTransp,
            LocalDateTime dataInicio, LocalDateTime dataFim) {
        return this.encomendas
                .getListRegistoUtilizadorEspacoTempo(this.utilizadorActual.getEmail(), dataInicio,
                        dataFim)
                .stream()
                .filter(r -> r.getCodTransportador() != null
                        && r.getCodTransportador().equals(codTransp))
                .map(RegistoEncomenda::clone).collect(Collectors.toList());
    }

    // #endregion

}

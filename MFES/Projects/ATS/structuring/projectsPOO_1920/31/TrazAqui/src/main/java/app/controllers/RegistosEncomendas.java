package app.controllers;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import app.enums.EstadosEncomendaEnum;
import app.exceptions.EncomendaJaCanceladaException;
import app.exceptions.EncomendaNaoExistenteException;
import app.exceptions.EstadoRegressivoException;
import app.models.Encomenda;
import app.models.RegistoEncomenda;

public class RegistosEncomendas implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = -1483564993341797854L;
    // #region variables
    private Map<RegistoEncomenda, List<String>> encomendas;
    private static String errorMessageEncomendaNaoExistente = "Encomenda não existente !!\n";
    private static String errorMessageEncomendaJaCancelada = "Encomenda já cancelada !!\n";
    private static String errorMessageEncomendaEstadoRegressivo =
            "Encomenda não pode atualizar para esse estado !!\n";

    // #endregion

    // #region Construtores

    public RegistosEncomendas() {
        this.encomendas = new TreeMap<>();
    }

    /**
     * @param encomendas
     */
    public RegistosEncomendas(Map<RegistoEncomenda, List<String>> encomendas) {
        this.setEncomendas(encomendas);
    }

    /**
     * @param r
     */
    public RegistosEncomendas(RegistosEncomendas r) {
        this.setEncomendas(r.encomendas);
    }
    // #endregion

    // #region getters setter
    /**
     * @return the encomendas
     */
    public Map<RegistoEncomenda, List<String>> getEncomendas() {
        return this.encomendas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey,
                Map.Entry::getValue, (oldValue, newValue) -> newValue, TreeMap::new));
    }

    /**
     * @param encomendas the encomendas to set
     */
    public void setEncomendas(Map<RegistoEncomenda, List<String>> encomendas) {
        this.encomendas = encomendas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey,
                Map.Entry::getValue, (oldValue, newValue) -> newValue, TreeMap::new));
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
        RegistosEncomendas registo = (RegistosEncomendas) o;
        // field comparison
        return Objects.equals(this.encomendas, registo.encomendas);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomendas: ");
        sb.append('\n');
        for (Entry<RegistoEncomenda, List<String>> entry : this.encomendas.entrySet()) {
            sb.append("Encomenda: ");
            sb.append('\n');
            sb.append((entry.getKey()).toString());
            sb.append('\n');
            sb.append("Código de Transportadores: ");
            sb.append('\n');
            for (String string : entry.getValue()) {
                sb.append(string);
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    public RegistosEncomendas clone() {
        return new RegistosEncomendas(this);
    }

    // #endregion

    // #region Methods

    public void adicionaRegistoEncomenda(RegistoEncomenda r) {
        this.encomendas.put(r.clone(), new ArrayList<>());
    }

    public void adicionaRegistoEncomenda(RegistoEncomenda r, List<String> lista) {
        this.encomendas.put(r.clone(), lista);
    }

    public boolean existeEncomenda(RegistoEncomenda r) {

        return this.encomendas.containsKey(r);

    }

    public boolean existeEncomenda(String codEncomenda) {

        return this.encomendas.keySet().stream()
                .anyMatch(r -> r.getEncomenda().getCodEnc().equals(codEncomenda));

    }

    public RegistoEncomenda getRegistoEncomenda(String codEncomenda)
            throws EncomendaNaoExistenteException {
        Optional<RegistoEncomenda> optEnc = this.encomendas.keySet().stream()
                .filter(r -> r.getEncomenda().getCodEnc().equals(codEncomenda)).findAny();
        RegistoEncomenda enc = null;
        if (optEnc.isPresent()) {
            enc = optEnc.get().clone();
        } else {
            throw new EncomendaNaoExistenteException(errorMessageEncomendaNaoExistente);
        }
        return enc;
    }

    public RegistoEncomenda getRegistoEncomenda(Encomenda encomenda)
            throws EncomendaNaoExistenteException {

        return this.getRegistoEncomenda(encomenda.getCodEnc());

    }

    public void apagaRegistoEncomenda(String codEncomenda) throws EncomendaNaoExistenteException {
        if (existeEncomenda(codEncomenda)) {
            this.encomendas.remove(this.getRegistoEncomenda(codEncomenda));
        } else {
            throw new EncomendaNaoExistenteException(errorMessageEncomendaNaoExistente);
        }
    }

    public void apagaRegistoEncomenda(RegistoEncomenda registoEncomenda)
            throws EncomendaNaoExistenteException {
        if (existeEncomenda(registoEncomenda)) {
            this.encomendas.remove(registoEncomenda);
        } else {
            throw new EncomendaNaoExistenteException(errorMessageEncomendaNaoExistente);
        }
    }

    public void atualizaRegisto(RegistoEncomenda r) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {

        this.atualizaEstado(r.getEncomenda().getCodEnc(), r.getEstadoEncomenda());
        List<String> lista = new ArrayList<>(this.encomendas.get(r));
        this.apagaRegistoEncomenda(r.getEncomenda().getCodEnc());
        this.adicionaRegistoEncomenda(r, lista);
    }

    private void atualizaEstado(String codEncomenda, EstadosEncomendaEnum estado)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException {

        RegistoEncomenda enc = this.getRegistoEncomenda(codEncomenda);

        if (enc.getEstadoEncomenda() == EstadosEncomendaEnum.CANCELADA) {
            throw new EncomendaJaCanceladaException(errorMessageEncomendaJaCancelada);
        }

        if (enc.getEstadoEncomenda().ordinal() > estado.ordinal()) {
            throw new EstadoRegressivoException(errorMessageEncomendaEstadoRegressivo);
        }

        enc.setEstadoEncomenda(estado);
        if (estado == EstadosEncomendaEnum.AGUARDAENVIO && enc.getDataAguardaEnvio() == null) {
            enc.setDataAguardaEnvio(LocalDateTime.now());
        } else if (estado == EstadosEncomendaEnum.ENVIADA && enc.getDataEnviada() == null) {
            enc.setDataEnviada(LocalDateTime.now());
        } else if (estado == EstadosEncomendaEnum.RECEBIDA && enc.getDataEntregue() == null) {
            enc.setDataEntregue(LocalDateTime.now());
        }
        List<String> lista = new ArrayList<>(this.encomendas.get(enc));

        this.apagaRegistoEncomenda(enc.getEncomenda().getCodEnc());
        this.adicionaRegistoEncomenda(enc, lista);

    }

    public void sinalizaEncomendaPronta(String codEnc) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {
        this.atualizaEstado(codEnc, EstadosEncomendaEnum.AGUARDAENVIO);
    }

    public void sinalizaEncomendaTransporteDisponivel(String codEnc, String codTransportador)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException {
        RegistoEncomenda enc = this.getRegistoEncomenda(codEnc);
        List<String> lista = new ArrayList<>(this.encomendas.get(enc));
        if (!lista.contains(codTransportador)) {
            lista.add(codTransportador);
        }
        this.encomendas.put(enc, lista);
        this.atualizaEstado(codEnc, EstadosEncomendaEnum.AGUARDAAPROVACAO);

    }

    public void sinalizaEncomendaTransporteEnvio(String codEnc, String codTransportador)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException {
        RegistoEncomenda enc = this.getRegistoEncomenda(codEnc);
        List<String> lista = new ArrayList<>();
        lista.add(codTransportador);
        this.apagaRegistoEncomenda(enc);
        enc.setCodTransportador(codTransportador);
        this.encomendas.put(enc, lista);
        this.atualizaEstado(codEnc, EstadosEncomendaEnum.APROVADA);

    }

    public void sinalizaEnvio(String codEnc) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {
        this.atualizaEstado(codEnc, EstadosEncomendaEnum.ENVIADA);

    }

    public void sinalizaRececao(String codEnc) throws EncomendaNaoExistenteException,
            EncomendaJaCanceladaException, EstadoRegressivoException {
        this.atualizaEstado(codEnc, EstadosEncomendaEnum.RECEBIDA);

    }

    public void classificaEntrega(String codEnc, int classificacao)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException {
        RegistoEncomenda enc = this.getRegistoEncomenda(codEnc);
        if (enc.getEstadoEncomenda() == EstadosEncomendaEnum.RECEBIDA
                && enc.getClassificacao() == -1 && (classificacao >= 0 && classificacao <= 10)) {
            enc.setClassificacao(classificacao);
            this.atualizaRegisto(enc);
        }
    }

    // Conta Registos pelo estado

    public long contaRegistosEstado(EstadosEncomendaEnum estado) {
        long count = 0;
        if (estado == EstadosEncomendaEnum.ABERTA) {
            count = this.encomendas.keySet().stream()
                    .filter(e -> e.getEstadoEncomenda() == EstadosEncomendaEnum.ABERTA).count();
        } else if (estado == EstadosEncomendaEnum.AGUARDAENVIO) {
            count = this.encomendas.keySet().stream()
                    .filter(e -> e.getEstadoEncomenda() == EstadosEncomendaEnum.AGUARDAENVIO)
                    .count();

        } else if (estado == EstadosEncomendaEnum.ENVIADA) {
            count = this.encomendas.keySet().stream()
                    .filter(e -> e.getEstadoEncomenda() == EstadosEncomendaEnum.ENVIADA).count();

        } else if (estado == EstadosEncomendaEnum.RECEBIDA) {
            count = this.encomendas.keySet().stream()
                    .filter(e -> e.getEstadoEncomenda() == EstadosEncomendaEnum.RECEBIDA).count();

        } else if (estado == EstadosEncomendaEnum.CANCELADA) {
            count = this.encomendas.keySet().stream()
                    .filter(e -> e.getEstadoEncomenda() == EstadosEncomendaEnum.CANCELADA).count();

        } else if (estado == EstadosEncomendaEnum.TODAS) {
            count = this.encomendas.keySet().stream().count();

        }
        return count;
    }

    public List<RegistoEncomenda> getListRegistoPorEstado(String codUtilizador,
            EstadosEncomendaEnum estado) {
        return this.encomendas.keySet().stream()
                .filter(e -> e.getEncomenda().getCodUtilizador().equals(codUtilizador)
                        && e.getEstadoEncomenda() == estado)
                .map(RegistoEncomenda::clone).collect(Collectors.toList());
    }

    public List<String> getListaTransportadoresEnc(String codEnc)
            throws EncomendaNaoExistenteException {
        RegistoEncomenda enc = this.getRegistoEncomenda(codEnc);
        return new ArrayList<>(this.encomendas.get(enc));
    }

    public List<RegistoEncomenda> getListRegistoPorEstado(EstadosEncomendaEnum estado) {
        return this.encomendas.keySet().stream().filter(e -> e.getEstadoEncomenda() == estado)
                .map(RegistoEncomenda::clone).collect(Collectors.toList());
    }

    public List<RegistoEncomenda> getListRegistoUtilizadorEspacoTempo(String codUtilizador,
            LocalDateTime inicio, LocalDateTime fim) {
        return this.encomendas.keySet().stream()
                .filter(e -> e.getEncomenda().getCodUtilizador().equals(codUtilizador)
                        && e.getDataEnviada() != null && e.getDataEnviada().isAfter(inicio)
                        && e.getDataEnviada().isBefore(fim))
                .map(RegistoEncomenda::clone).collect(Collectors.toList());
    }

    // #endregion

}

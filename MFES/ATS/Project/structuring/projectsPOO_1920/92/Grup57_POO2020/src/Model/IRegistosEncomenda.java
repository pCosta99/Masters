package Model;

import java.util.List;

public interface IRegistosEncomenda {
    List<RegistoEncomenda> extraiRegistosDeAlguem(String quemFoi);

    void insertNosRegistos(RegistoEncomenda r);

    RegistosEncomenda exportRegistos();
}

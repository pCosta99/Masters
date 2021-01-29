package hedgehog.control;

import hedgehog.model.account.Account;
import hedgehog.model.account.EmailTraits;
import hedgehog.model.account.PasswordTraits;
import hedgehog.model.account.UsernameTraits;
import hedgehog.model.client.Client;
import hedgehog.model.emissary.Firm;
import hedgehog.util.maybe.Maybe;
import hedgehog.util.point.Point;
import hedgehog.util.result.Result;
import hedgehog.view.GUI;
import hedgehog.model.emissary.Volunteer;
import hedgehog.model.order.Order;
import hedgehog.model.parser.Parser;
import hedgehog.model.store.Store;

import java.util.List;
import java.util.Random;

import static hedgehog.util.maybe.Maybe.Nothing;

public class InterfaceController {
    private static final Random rng = new Random();
    private Parser file_parser;
    private Database database;
    private GUI view;

    public InterfaceController(final Database database, final Parser parser, final GUI view) {
        this.file_parser = parser;
        this.database = database;
        this.view = view;
    }

    public void startController() {
        char option;
        String buffer;

        // ciclo do programa
        do {
            // imprime a janela
            this.view.menuTop();
            this.view.menuMidMainStart();
            this.view.menuBot();

            // recebe a opção do utilizador
            buffer = Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }



            // seleciona o que fazer em função da opção
            switch (option) {
                case 'Y':
                    this.view.menuTop();
                    this.view.menuMidIO("Indique o nome do ficheiro");
                    this.view.menuBot();

                    final var filename = Input.lerString();
                    final var parse_result = this.load_database_file(filename);

                    if (parse_result.is_err()) {
                        System.out.print("Erro ao carregar o ficheiro: ");
                        switch (parse_result.unwrap_err()) {
                        case FILE_NOT_FOUND:
                            System.out.print("ficheiro não encontrado");
                            break;
                        case IO_ERR:
                            System.out.print("erro ao ler do ficheiro");
                            break;
                        }
                        break;
                    }
                case 'N':
                default:
                    menuMid();
                    break;
            }
        } while (option != 'X');
    }

    private void menuMid() {
        this.view.menuTop();
        this.view.menuMidMain();
        this.view.menuBot();
        
        final var buffer = Input.lerString();
        char option;
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
        case '1':
            optCliente();
            break;
        case '2':
            optVoluntario();
            break;
        case '3':
            optEmpresa();
            break;
        case '4':
            optLoja();
            break;
        default:
            break;
        }
    }

    private Result<Parser.Info, Parser.FatalError>
    load_database_file(final String filename) {
        return this.file_parser.parse(filename);
    }

    /**
     * Pagina no ecra uma lista de strings.
     *
     * @param tamanho  tamanho total da lista recebida
     * @param direcoes contem os indices (previous,next)
     * @param option   sentido do movimento da paginacao
     */
    public void pagina(int tamanho, List<Integer> direcoes, char option) {
        switch (option) {
            case 'N':
                if (tamanho - direcoes.get(1) != 0) {
                    if (tamanho - direcoes.get(1) < 11) {
                        direcoes.set(0, direcoes.get(1));
                        direcoes.set(1, tamanho);
                    } else {
                        direcoes.set(0, direcoes.get(0) + 11);
                        direcoes.set(1, direcoes.get(1) + 11);
                    }
                }
                break;
            case 'P':
                if (tamanho - direcoes.get(1) != 0) {
                    if (direcoes.get(0) > 11) {
                        direcoes.set(0, direcoes.get(0) - 11);
                        direcoes.set(1, direcoes.get(1) - 11);
                    } else {
                        direcoes.set(0, 0);
                        direcoes.set(1, 10);
                    }
                } else {
                    direcoes.set(1, Math.max(10, direcoes.get(0)));
                    direcoes.set(0, Math.max(direcoes.get(0) - 10, 0));
                }
                break;
            default:
                break;
        }
    }

    public void optCliente() {
        //imprime a janela
        this.view.menuTop();
        this.view.menuMidCliente();
        this.view.menuBot();
        
        //recebe a opção do utilizador
        final var buffer = Input.lerString();
        char option;
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case '1':
                optClienteLogin();
                break;
            case '2':
                optClienteRegistar();
                break;
            default:
                break;
        }
    }

    public void optClienteLogin() {
        // validating credentials
        for (;;) {
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma PassWord:");
            this.view.menuBot();
            final var password = Input.lerString();

            final var maybe_client = this.database.find_client(email);

            if (maybe_client.is_nothing()) {
                System.out.println("no such user");
            } else if (!maybe_client.unwrap().account().password_matches(password.toCharArray())) {
                System.out.println("invalid password");
            } else {
                break;
            }
        }
        menuClientLog();
    }

    private void optAcederInformacoes() {
        this.view.menuTopQueries();
        this.view.menuMidIO("Insira Voluntário ou Empresa:");
        this.view.menuBot();
        final var emissary_code = Input.lerString();

        if (emissary_code.length() < 2) {
            menuClientLog();
        }

        int code;
        try {
            code = Integer.parseInt(emissary_code.substring(1));
        } catch (NumberFormatException e) {
            System.out.println("Identificador inválido");
            menuClientLog();
            throw new RuntimeException();
        }

        Maybe<Firm> maybe_firm = Nothing();
        Maybe<Volunteer> maybe_volunteer = Nothing();
        boolean is_firm;
        switch (Character.toUpperCase(emissary_code.charAt(0))) {
        case 'T':
            maybe_firm = this.database.find_firm(code);
            is_firm = true;
            break;
        case 'V':
            maybe_volunteer = this.database.find_volunteer(code);
            is_firm = false;
            break;
        default:
            System.out.println("Identificador inválido");
            menuClientLog();
            break;
        }

        if (maybe_firm.is_just() || maybe_volunteer.is_just()) {
            this.view.menuTop();
            this.view.menuMidIO("Insira um intervalo de tempo [dia/mês/ano]:");
            this.view.menuBot();
            final var time_period = Input.lerDouble();

            this.view.menuTopQueries();
            this.view.menuEncomendasFeitas(null, null);
            this.view.menuBot();

            // final var total = encomendas_feitas.count();
            // List<Integer> direcoes = new ArrayList<>(List.of(0,10));

            // this.view.menuEncomendasFeitas(
            //     encomendas_feitas.subList(direcoes.get(0), direcoes.get(1))
            // );
        } else {
            System.out.println("Nenhuma encontrada");
        }

        // pagina(total, direcoes, option);
    }

    private void menuClientLog() {
        char option;
        do {
            this.view.menuTop();
            this.view.menuMidClienteLog();
            this.view.menuBotStats();

            //recebe a opção do utilizador
            final var buffer = Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            //pagina as opçoes de utilizador
            switch (option) {
                case '1':
                    // optSolicitarEntrega();
                    break;
                case '2':
                    optEfetuarEncomenda();
                    break;
                case '3':
                    optClassificarServiço();
                    break;
                case '4':
                    optAcederInformacoes();
                    break;
                //case 'S':
                    // model.salvaEstado(model);
                    //  break;
                default:
                    break;
            }
        } while (option != 'X');
    }

    private void optClassificarServiço() {
        this.view.menuTopQueries();
        this.view.menuMidIO("Insira Voluntário ou Empresa:");
        this.view.menuBot();
        final var emissary_code = Input.lerString();

        if (emissary_code.length() < 2) {
            menuClientLog();
        }

        int code;
        try {
            code = Integer.parseInt(emissary_code.substring(1));
        } catch (NumberFormatException e) {
            System.out.println("Identificador inválido");
            menuClientLog();
            throw new RuntimeException();
        }

        Maybe<Firm> maybe_firm = Nothing();
        Maybe<Volunteer> maybe_volunteer = Nothing();
        boolean is_firm;
        switch (Character.toUpperCase(emissary_code.charAt(0))) {
        case 'T':
            maybe_firm = this.database.find_firm(code);
            is_firm = true;
            break;
        case 'V':
            maybe_volunteer = this.database.find_volunteer(code);
            is_firm = false;
            break;
        default:
            System.out.println("Identificador inválido");
            menuClientLog();
            throw new RuntimeException();
        }

        if (maybe_firm.is_just() || maybe_volunteer.is_just()) {
            this.view.menuTop();
            this.view.menuMidIO("Insira uma Avaliação de 0(Péssimo) a 5(Excelente):");
            this.view.menuBot();
            final var rating = Input.lerDouble();

            if (is_firm) {
                final var rate_result = maybe_firm.unwrap().rate(rating);
                if (rate_result.is_err()) {
                    switch (rate_result.unwrap_err()) {
                        case RATING_BELOW_MIN:
                            System.out.println("Avaliação abaixo do valor mínimo");
                            break;
                        case RATING_OVER_MAX:
                            System.out.println("Avaliação acima do valor máximo");
                        default:
                            break;
                    }
                }
            } else {
                final var rate_result = maybe_volunteer.unwrap().rate(rating);
                if (rate_result.is_err()) {
                    switch (rate_result.unwrap_err()) {
                        case RATING_BELOW_MIN:
                            System.out.println("Avaliação abaixo do valor mínimo");
                            break;
                        case RATING_OVER_MAX:
                            System.out.println("Avaliação acima do valor máximo");
                        default:
                            break;
                    }
                }
            }
            menuClientLog();
        }
    }

    public void optClienteRegistar() {
        for (;;) {
            this.view.menuTop();
            this.view.menuMidIO("Insira um Username:");
            this.view.menuBot();
            final var username = Input.lerString();
            if (UsernameTraits.check_username(username).is_err()) {
                System.out.println("Username inválido");
                continue;
            }

            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();
            if (EmailTraits.check_email(email).is_err()) {
                System.out.println("E-mail inválido");
                continue;
            }
            if (this.database.client_exists(email)) {
                System.out.println("E-mail em uso");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();
            if (PasswordTraits.check_password(password).is_err()) {
                System.out.println("Password inválida");
                continue;
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
            this.view.menuBot();
            final double x = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
            this.view.menuBot();
            final double y = Input.lerDouble();

            int generated_client_code;
            for (;;) {
                generated_client_code = rng.nextInt(Integer.MAX_VALUE);
                if (!this.database.client_exists(generated_client_code)) {
                    break;
                }
            }

            this.database.register_client(new Client(
                generated_client_code,
                Account.of(username, email, password).unwrap(),
                Point.at(x, y))
            );
            break;
        }
        this.view.menuTopQueries();
        this.view.menuMidIO("Registado com sucesso");
        this.view.menuBot();
    }

    public void optEfetuarEncomenda() {
        for (;;) {
            this.view.menuTop();
            this.view.menuMidIO("Insira a Loja:");
            this.view.menuBot();
            final var store_id = Input.lerString();
            if (store_id.length() < 2) {
                System.out.println("Loja inválida");
                continue;
            }
            int store_code;
            try {
                store_code = Integer.parseInt(store_id.substring(1));
            } catch (NumberFormatException e) {
                System.out.println("Loja inválida");
                continue;
            }

            final var maybe_store = this.database.find_store(store_code);

            if (maybe_store.is_nothing()) {
                System.out.println("Loja não existente");
                continue;
            }

            this.view.menuTopQueries();
            this.view.menuMidIO("Insira a encomenda:");
            this.view.menuBot();
            final var order_description = Input.lerString();

            int generated_order_code;
            for (;;) {
                generated_order_code = rng.nextInt(Integer.MAX_VALUE);
                if (!this.database.order_exists(generated_order_code)) {
                    break;
                }
            }

            this.database.register_order(new Order(
                new Client(
                    42,
                    Account.of("default", "default@client.trazaqui.com", "P@ssword123").unwrap(),
                    Point.at(42, 42)
                ),
                maybe_store.unwrap(),
                0,
                generated_order_code
            ));
            break;
        }
        this.view.menuTopQueries();
        this.view.menuMidIO("Registado com sucesso");
        this.view.menuBot();
    }

    public void optVoluntarioLogin() {
        for (;;) {
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();

            final var maybe_volunteer = this.database.find_volunteer(email);

            if (maybe_volunteer.is_nothing()) {
                System.out.println("no such user");
            } else if (!maybe_volunteer.unwrap().account().password_matches(password.toCharArray())) {
                System.out.println("invalid password");
            } else {
                break;
            }
        }
        menuVoluntarioLog();
    }

    public void optVoluntarioRegistar() {
        for (;;) {
            this.view.menuTop();
            this.view.menuMidIO("Insira um Username:");
            this.view.menuBot();
            final var username = Input.lerString();
            if (UsernameTraits.check_username(username).is_err()) {
                System.out.println("Username inválido");
                continue;
            }

            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();
            if (EmailTraits.check_email(email).is_err()) {
                System.out.println("E-mail inválido");
                continue;
            }
            if (this.database.volunteer_exists(email)) {
                System.out.println("E-mail em uso");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();
            if (PasswordTraits.check_password(password).is_err()) {
                System.out.println("Password inválida");
                continue;
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
            this.view.menuBot();
            final double x = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
            this.view.menuBot();
            final double y = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira a sua velocidade média");
            this.view.menuBot();
            final double base_delivery_speed = Input.lerDouble();
            if (base_delivery_speed < 0.) {
                System.out.println("Velocidade média tem de ser positiva");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira o seu raio de ação");
            this.view.menuBot();
            final double delivery_range = Input.lerDouble();
            if (delivery_range < 0.) {
                System.out.println("Raio de ação tem de ser positiva");
            }

            int generated_volunteer_code;
            for (;;) {
                generated_volunteer_code = rng.nextInt(Integer.MAX_VALUE);
                if (!this.database.volunteer_exists(generated_volunteer_code)) {
                    break;
                }
            }

            this.database.register_volunteer(Volunteer.of(
                generated_volunteer_code,
                Account.of(username, email, password).unwrap(),
                Point.at(x, y),
                base_delivery_speed,
                delivery_range
            ).unwrap());
            break;
        }
        this.view.menuTopQueries();
        this.view.menuMidIO("Registado com sucesso");
        this.view.menuBot();
    }

    public void optVoluntario() {
        //imprime a janela
        this.view.menuTop();
        this.view.menuMidVoluntario();
        this.view.menuBot();

        //recebe a opção do utilizador
        final var buffer = Input.lerString();
        char option;
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case '1':
                optVoluntarioLogin();
                break;
            case '2':
                optVoluntarioRegistar();
                break;
            default:
                break;
        }
    }

    private void menuVoluntarioLog() {
        char option;
        do {
            this.view.menuTop();
            this.view.menuMidVoluntarioLog();
            this.view.menuBotStats();

            //recebe a opção do utilizador
            final var buffer = Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            //pagina as opçoes de utilizador
            switch (option) {
                case '1':
                    // optSinalizarDisponibilidade();
                    break;
                case '2':
                    // optVerEncomendas();
                    break;
                case '3':
                    // optRegistarDuracao();
                    break;
                default:
                    break;
            }
        } while (option != 'X');
    }

    public void optEmpresaLogin() {
        for (;;) {
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();

            final var maybe_firm = this.database.find_firm(email);

            if (maybe_firm.is_nothing()) {
                System.out.println("no such user");
            } else if (!maybe_firm.unwrap().account().password_matches(password.toCharArray())) {
                System.out.println("invalid password");
            } else {
                break;
            }
        }
        menuEmpresaLog();
    }

    public void optEmpresaRegistar() {
        for (;;) {
            this.view.menuTop();
            this.view.menuMidIO("Insira um Username:");
            this.view.menuBot();
            final var username = Input.lerString();
            if (UsernameTraits.check_username(username).is_err()) {
                System.out.println("Username inválido");
                continue;
            }

            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();
            if (EmailTraits.check_email(email).is_err()) {
                System.out.println("E-mail inválido");
                continue;
            }
            if (this.database.firm_exists(email)) {
                System.out.println("E-mail em uso");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();
            if (PasswordTraits.check_password(password).is_err()) {
                System.out.println("Password inválida");
                continue;
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
            this.view.menuBot();
            final double x = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
            this.view.menuBot();
            final double y = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira a sua velocidade média");
            this.view.menuBot();
            final double base_delivery_speed = Input.lerDouble();
            if (base_delivery_speed < 0.) {
                System.out.println("Velocidade média tem de ser positiva");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira o seu raio de ação");
            this.view.menuBot();
            final double delivery_range = Input.lerDouble();
            if (delivery_range < 0.) {
                System.out.println("Raio de ação tem de ser positiva");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira o seu imposto (€/km)");
            this.view.menuBot();
            final double distance_fare = Input.lerDouble();
            if (distance_fare < 0.) {
                System.out.println("Imposto tem de ser positivo");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira o seu nif");
            this.view.menuBot();
            final int vat = Input.lerInt();

            int generated_firm_code;
            for (;;) {
                generated_firm_code = rng.nextInt(Integer.MAX_VALUE);
                if (!this.database.firm_exists(generated_firm_code)) {
                    break;
                }
            }

            this.database.register_firm(Firm.of(
                generated_firm_code,
                Account.of(username, email, password).unwrap(),
                Point.at(x, y),
                vat,
                base_delivery_speed,
                delivery_range,
                distance_fare
            ).unwrap());
            break;
        }
        this.view.menuTopQueries();
        this.view.menuMidIO("Registado com sucesso");
        this.view.menuBot();
    }

    public void optEmpresa() {
        //imprime a janela
        this.view.menuTop();
        this.view.menuMidEmpresa();
        this.view.menuBot();

        //recebe a opção do utilizador
        final var buffer = Input.lerString();
        char option;
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case '1':
                optEmpresaLogin();
                break;
            case '2':
                optEmpresaRegistar();
                break;
            default:
                break;
        }
    }

    private void menuEmpresaLog() {
        char option;
        do {
            this.view.menuTop();
            this.view.
            menuMidEmpresaLog();
            this.view.menuBotStats();

            //recebe a opção do utilizador
            final var buffer = Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            //pagina as opçoes de utilizador
            switch (option) {
                case '1':
                    // optSinalizarDisponibilidade();
                    break;
                case '2':
                    // optVerEncomendas();
                    break;
                case '3':
                    // optRegistarDuracao();
                    break;
                default:
                    break;
            }
        } while (option != 'X');
    }

    public void optLojaLogin() {
        for (;;) {
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();

            final var maybe_store = this.database.find_store(email);

            if (maybe_store.is_nothing()) {
                System.out.println("no such user");
            } else if (!maybe_store.unwrap().account().password_matches(password.toCharArray())) {
                System.out.println("invalid password");
            } else {
                break;
            }
        }
        menuLojaLog();
    }

    public void optLojaRegistar() {
        for (;;) {
            this.view.menuTop();
            this.view.menuMidIO("Insira um Username:");
            this.view.menuBot();
            final var username = Input.lerString();
            if (UsernameTraits.check_username(username).is_err()) {
                System.out.println("Username inválido");
                continue;
            }

            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();
            final var email = Input.lerString();
            if (EmailTraits.check_email(email).is_err()) {
                System.out.println("E-mail inválido");
                continue;
            }
            if (this.database.store_exists(email)) {
                System.out.println("E-mail em uso");
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Password:");
            this.view.menuBot();
            final var password = Input.lerString();
            if (PasswordTraits.check_password(password).is_err()) {
                System.out.println("Password inválida");
                continue;
            }

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
            this.view.menuBot();
            final double x = Input.lerDouble();

            this.view.menuTop();
            this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
            this.view.menuBot();
            final double y = Input.lerDouble();

            int generated_store_code;
            for (;;) {
                generated_store_code = rng.nextInt(Integer.MAX_VALUE);
                if (!this.database.store_exists(generated_store_code)) {
                    break;
                }
            }

            this.database.register_store(new Store(
                generated_store_code,
                Account.of(username, email, password).unwrap(),
                Point.at(x, y)
            ));
            break;
        }
        this.view.menuTopQueries();
        this.view.menuMidIO("Registado com sucesso");
        this.view.menuBot();
    }

    public void optLoja() {
        //imprime a janela
        this.view.menuTop();
        this.view.menuMidEmpresa();
        this.view.menuBot();

        //recebe a opção do utilizador
        final var buffer = Input.lerString();
        char option;
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case '1':
                optLojaLogin();
                break;
            case '2':
                optLojaRegistar();
                break;
            default:
                break;
        }
    }

    private void menuLojaLog() {
        char option;
        do {
            this.view.menuTop();
            this.view.menuMidLojaLog();
            this.view.menuBotStats();

            //recebe a opção do utilizador
            final var buffer = Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            //pagina as opçoes de utilizador
            switch (option) {
                case '1':
                    // optSinalizarDisponibilidade();
                    break;
                case '2':
                    // optVerEncomendas();
                    break;
                case '3':
                    // optRegistarDuracao();
                    break;
                default:
                    break;
            }
        } while (option != 'X');
    }
}

/*     public void optVoluntario() {
        char option;
            do {
                //imprime a janela
                this.view.menuTopQueries();
                this.view.menuMidVoluntario();
                this.view.menuBot();

                //recebe a opção do Voluntario
                buffer = control.Input.lerString();
                if (buffer.length() > 0) {
                    option = Character.toUpperCase(buffer.charAt(0));
                } else {
                    option = ' ';
                }

                switch (option) {
                    case '1':
                        optVoluntarioLogin();
                        break;
                    case '2':
                        optVoluntarioRegistar();
                        break;
                    default:
                        break;

                }
                while (option != 'X') ;
            }
        }

    public void optVoluntarioLogin() {
            String email;
            String buffer;


            do {
                //imprime a janela
                this.view.menuTopQueries();
                this.view.menuMidIO("Insira um E-Mail:");
                this.view.menuBot();

                email = control.Input.lerString();

                String password;
                { //imprime a janela
                    this.view.menuTop();
                    this.view.menuMidIO("Insira uma PassWord:");
                    this.view.menuBot();
                    password = control.Input.lerString();
                }
                while (!this.database.volunteer_exists(email,password)) ;

                do {
                    this.view.menuTop();
                    this.view.menuMidVoluntarioLog();
                    this.view.menuBot();


                    //recebe a opção do Voluntario
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //pagina as opçoes de Voluntario
                    switch (option) {
                        case '1':
                            optSinalizarDisponibilidadeVoluntario();
                            break;
                        case '2':
                            optVerEncomendasDisponiveis();
                            break;
                        case '3':
                            optRegistarDuracao();
                            break;
                        case 'S':
                            model.salvaEstado(model);
                            break;
                        default:
                            break;
                    }
                }
            }
        }

    public void optVoluntarioRegistar() {
        Volunteer v = new Volunteer();
        char option;
        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidVoluntario();
            this.view.menuBot();

            String name {
                //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira o seu Nome:");
                this.view.menuBot();
                name = control.Input.lerString();
            }

            //random do code

            //random da base_delivery_speed

            //delivery range?

            int option {
                //imprime a janela
                this.view.menuTop();
                System.out.println("O voluntário é Medically Certified? 1-Sim , 2-Não");
                this.view.menuBot();
                boolean aux;
                switch (op) {
                    case 1:
                        aux = true;
                        break;

                    case 2:
                        aux = false;
                        break;

                    default:
                        break;
                }
                while (option != 'X') ;
            }

            Double locationx;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
                this.view.menuBot();
                locationx = control.Input.lerDouble();
            }
            Double locationy;
            {
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
                this.view.menuBot();
                locationy = control.Input.lerDouble();
            }

            if (database.volunteer_exists == true) {
                //imprime a janela
                this.view.menuTopQueries();
                this.view.menuMidIO("E-Mail já Registado");
                this.view.menuBot();
                do {
                    //imprime a janela
                    this.view.menuTopQueries();
                    this.view.menuMidVolunteer();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }
                    switch (option) {
                        case '1':
                            optVoluntarioLogin();
                            break;
                        case '2':
                            optVoluntarioRegistar();
                            break;
                        default:
                            break;

                    }
                    while (option != 'X') ;
                } else{
                    this.view.menuTopQueries();
                    this.view.menuMidIO("Registado com sucesso");
                    this.view.menuBot();

                    Volunteer vol = new Volunteer(name, code, 0.0, 0.0, 0.0, aux, (locationx, locationy),
                    0.0, NULL) //SEM CERTEZA NO ULTIMO CAMPO
                    database.register_volunteer(vol);

                    do {
                        //imprime a janela
                        this.view.menuTopQueries();
                        this.view.menuMidVolunteer();
                        this.view.menuBot();

                        //recebe a opção do Voluntário
                        buffer = control.Input.lerString();
                        if (buffer.length() > 0) {
                            option = Character.toUpperCase(buffer.charAt(0));
                        } else {
                            option = ' ';
                        }

                        switch (option) {
                            case '1':
                                optVoluntarioLogin();
                                break;
                            case '2':
                                optVoluntarioRegistar();
                                break;
                            default:
                                break;

                        }
                        while (option != 'X') ;
                    }
                }
            }
        }
    }

}

    public void optEmpresa() {
        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidEmpresa();
            this.view.menuBot();

            //recebe a opção da Empresa
            buffer = control.Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            switch (option) {
                case '1':
                    optEmpresaLogin();
                    break;
                case '2':
                    optEmpresaRegistar();
                    break;
                default:
                    break;

            }
            while (option != 'X') ;
        }
    }

    public void optEmpresaLogin() {
        String email;
        String buffer;


        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-Mail:");
            this.view.menuBot();

            email = control.Input.lerString();

            String password;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma PassWord:");
                this.view.menuBot();
                password = control.Input.lerString();
            }
            while (!this.database.firm_exists(email, password)) ;

            do {
                this.view.menuTop();
                this.view.menuMidEmpresaLog();
                this.view.menuBot();


                //recebe a opção da Empresa
                buffer = control.Input.lerString();
                if (buffer.length() > 0) {
                    option = Character.toUpperCase(buffer.charAt(0));
                } else {
                    option = ' ';
                }

                //pagina as opçoes de Empresa
                switch (option) {
                    case '1':
                        optSinalizarDisponibilidadeEmpresa();
                        break;
                    case '2':
                        optVerEncomendasDisponiveisEmpresa();
                        break;
                    case '3':
                        optRegistarDuracaoCusto();
                        break;
                    case 'S':
                        model.salvaEstado(model);
                        break;
                    default:
                        break;
                }
            }
        }
    }

    public void optEmpresaRegistar() {
        String email;
        String buffer;


        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-mail:");
            this.view.menuBot();

            email = control.Input.lerString();

            String username;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira um Username:");
                this.view.menuBot();
                username = control.Input.lerString();
            }
            Double locationx;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
                this.view.menuBot();
                locationx = control.Input.lerDouble();
            }
            Double locationy;
            {
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
                this.view.menuBot();
                locationy = control.Input.lerDouble();


            }
            Double delivery_range;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira um raio de Entrega:");
                this.view.menuBot();
                delivery_range = control.Input.lerDouble();
            }

            Double distance_fare;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira um Preço por Km:");
                this.view.menuBot();
                distance_fare = control.Input.lerDouble();
            }

            Boolean is_medically_certified;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira um Preço por Km:"); // fazer menu
                this.view.menuBot();
                is_medically_certified = control.Input.lerBoolean();
            }

            String password;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma PassWord:");
                this.view.menuBot();
                password = control.Input.lerString();
            }


            if (database.firm_exists == true) {
                //imprime a janela
                this.view.menuTopQueries();
                this.view.menuMidIO("E-Mail já Registado");
                this.view.menuBot();
                do {
                    //imprime a janela
                    this.view.menuTopQueries();
                    this.view.menuMidEmpresa();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    switch (option) {
                        case '1':
                            optEmpresaLogin();
                            break;
                        case '2':
                            optEmpresaRegistar();
                            break;
                        default:
                            break;

                    }
                    while (option != 'X') ;
                }
            } else {
                this.view.menuTopQueries();
                this.view.menuMidIO("Registado com sucesso");
                this.view.menuBot();

                //criar codigo nao existente

                Firm f = new Firm(username, email, password, (locationx, locationy), code )
                database.register_user(f);

                do {
                    //imprime a janela
                    this.view.menuTopQueries();
                    this.view.menuMidEmpresa();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    switch (option) {
                        case '1':
                            optEmpresaLogin();
                            break;
                        case '2':
                            optEmpresaRegistar();
                            break;
                        default:
                            break;

                    }
                    while (option != 'X') ;
                }
            }

        }
    }

    public void optLoja() {
        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidLoja();
            this.view.menuBot();

            //recebe a opção da Empresa
            buffer = control.Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

            switch (option) {
                case '1':
                    optLojaLogin();
                    break;
                case '2':
                    optLojaRegistar();
                    break;
                default:
                    break;

            }
            while (option != 'X') ;
        }
    }

    // queries
    public void optLojaLogin() {
        String email;
        String buffer;


        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidIO("Insira um E-Mail:");
            this.view.menuBot();

            email = control.Input.lerString();

            String password;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma PassWord:");
                this.view.menuBot();
                password = control.Input.lerString();
            }
            while (!this.database.store_exists(email, password)) ;

            do {
                this.view.menuTop();
                this.view.menuMidLojaLog();
                this.view.menuBot();


                //recebe a opção da Empresa
                buffer = control.Input.lerString();
                if (buffer.length() > 0) {
                    option = Character.toUpperCase(buffer.charAt(0));
                } else {
                    option = ' ';
                }

                //pagina as opçoes de Voluntario
                switch (option) {
                    case '1':
                        optSinalizarEncomendaDisponivel();
                        break;
                    case '2':
                        optIndicarFila();
                        break;
                    case 'S':
                        model.salvaEstado(model);
                        break;
                    default:
                        break;
                }
            }
        }
    }

    public void optLojaRegistar() {
        do {
            //imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidLoja();
            this.view.menuBot();

            String nome{
                //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira o Nome da Loja:");
                this.view.menuBot();
                nome = control.Input.lerString();
            }

            Double locationx;
            { //imprime a janela
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização x: (Como por exemplo: ´-97.28862´)");
                this.view.menuBot();
                locationx = control.Input.lerDouble();
            }
            Double locationy;
            {
                this.view.menuTop();
                this.view.menuMidIO("Insira uma Localização y: (Como por exemplo: ´59.067047´)");
                this.view.menuBot();
                locationy = control.Input.lerDouble();
            }

            ArrayDeque<Double> q = new ArrayDeque<>;

            if (database.store_exists == true) {
                //imprime a janela
                this.view.menuTopQueries();
                this.view.menuMidIO("E-Mail já Registado");
                this.view.menuBot();
                do {
                    //imprime a janela
                    this.view.menuTopQueries();
                    this.view.menuMidLoja();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    switch (option) {
                        case '1':
                            optLojaLogin();
                            break;
                        case '2':
                            optLojaRegistar();
                            break;
                        default:
                            break;

                    }
                    while (option != 'X') ;
                }
            } else {
                this.view.menuTopQueries();
                this.view.menuMidIO("Registado com sucesso");
                this.view.menuBot();

                //criar codigo nao existente

                Store s = new Store(code,nome,q,(locationx,locationy));
                database.register_Store(s);

                do {
                    //imprime a janela
                    this.view.menuTopQueries();
                    this.view.menuMidLoja();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    switch (option) {
                        case '1':
                            optLojaLogin();
                            break;
                        case '2':
                            optLojaRegistar();
                            break;
                        default:
                            break;

                    }
                    while (option != 'X') ;
                }
            }

        }
    }

    public void optSolicitarEntrega() {
                char option;
                String buffer;

                //ciclo do programa
                do {
                    //imprime a janela
                    this.view.menuTop();
                    this.view.menuMidSolicitarEntrega();
                    this.view.menuBot();

                    //recebe a opção do utilizador
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //seleciona o que fazer em função da opção
                    switch (option) {
                        case '1':
                            optAceitarEntrega();
                            break;
                        case '2':
                            do {
                                this.view.menuTop();
                                this.view.menuMidUtilizadorLog();
                                this.view.menuBot();


                                //recebe a opção do utilizador
                                buffer = control.Input.lerString();
                                if (buffer.length() > 0) {
                                    option = Character.toUpperCase(buffer.charAt(0));
                                } else {
                                    option = ' ';
                                }

                                //pagina as opçoes de utilizador
                                switch (option) {
                                    case '1':
                                        optSolicitarEntrega();
                                        break;
                                    case '2':
                                        optEfetuarEncomenda();
                                        break;
                                    case '3':
                                        optClassificarServiço();
                                        break;
                                    case '4':
                                        optAcederInformacoes();
                                        break;
                                    //case 'S':
                                    // model.salvaEstado(model);
                                    //  break;
                                    default:
                                        break;
                                }
                            }
                            break;
                        default:
                            break;
                    }
                } while (option != 'X');
            }

    public boolean optEfetuarEncomenda();

    {
        return true;
    }

}

    private void optAceitarEntrega() {

    }

public void optSinalizarDisponibilidadeVoluntario(){
    String buffer;
    char option;
    do {
        //imprime a janela
        this.view.menuTopQueries();
        this.view.menuSinalizarDisponibilidadeVoluntario();
        this.view.menuBot();

        //recebe a opção do utilizador
        buffer = control.Input.lerString();
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case 'Y':
                //mudar boolean para true;
                do {
                    this.view.menuTop();
                    this.view.menuMidVoluntarioLog();
                    this.view.menuBot();


                    //recebe a opção do Voluntario
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //pagina as opçoes de Voluntario
                    switch (option) {
                        case '1':
                            optSinalizarDisponibilidadeVoluntario();
                            break;
                        case '2':
                            optVerEncomendasDisponiveis();
                            break;
                        case '3':
                            optRegistarDuracao();
                            break;
                        case 'S':
                            model.salvaEstado(model);
                            break;
                        default:
                            break;
                    }
                }
                break;
            case 'N':
                do {
                    this.view.menuTop();
                    this.view.menuMidVoluntarioLog();
                    this.view.menuBot();


                    //recebe a opção do Voluntario
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //pagina as opçoes de Voluntario
                    switch (option) {
                        case '1':
                            optSinalizarDisponibilidadeVoluntario();
                            break;
                        case '2':
                            optVerEncomendasDisponiveis();
                            break;
                        case '3':
                            optRegistarDuracao();
                            break;
                        case 'S':
                            model.salvaEstado(model);
                            break;
                        default:
                            break;
                    }
                }
                break;
            default:
                break;

        }
        while (option != 'X') ;
    }

}

    public void optVerEncomendasDisponiveis() {
        String buffer;

        List<String> infoEncomendasDisponiveis = this.model.encomendasDisponiveis();
        do {
            // imprime a janela
            this.view.menuTopQueries();
            this.view.menuMidEncomendasDisponiveis(encomendasDisponiveis);
            this.view.menuBot();

            // recebe a opção do utilizador
            buffer = control.Input.lerString();
            if (buffer.length() > 0) {
                option = Character.toUpperCase(buffer.charAt(0));
            } else {
                option = ' ';
            }

        } while (option != 'X');

    }

    public void optRegistarDuracao() {
        // mudar valores em ficheiro
    }

public void optSinalizarDisponibilidadeEmpresa(){
    String buffer;
    char option;
    do {
        //imprime a janela
        this.view.menuTopQueries();
        this.view.menuSinalizarDisponibilidadeEmpresa();
        this.view.menuBot();

        //recebe a opção do utilizador
        buffer = control.Input.lerString();
        if (buffer.length() > 0) {
            option = Character.toUpperCase(buffer.charAt(0));
        } else {
            option = ' ';
        }

        switch (option) {
            case 'Y':
                //mudar boolean para true;
                do {
                    this.view.menuTop();
                    this.view.menuMidEmpresaLog();
                    this.view.menuBot();


                    //recebe a opção da Empresa
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //pagina as opçoes de Voluntario
                    switch (option) {
                        case '1':
                            optSinalizarDisponibilidadeEmpresa();
                            break;
                        case '2':
                            optVerEncomendasDisponiveisEmpresa();
                            break;
                        case '3':
                            optRegistarDuracaoCusto();
                            break;
                        case 'S':
                            model.salvaEstado(model);
                            break;
                        default:
                            break;
                    }
                }
                break;
            case 'N':
                do {
                    this.view.menuTop();
                    this.view.menuMidEmpresaLog();
                    this.view.menuBot();


                    //recebe a opção da Empresa
                    buffer = control.Input.lerString();
                    if (buffer.length() > 0) {
                        option = Character.toUpperCase(buffer.charAt(0));
                    } else {
                        option = ' ';
                    }

                    //pagina as opçoes de Voluntario
                    switch (option) {
                        case '1':
                            optSinalizarDisponibilidadeEmpresa();
                            break;
                        case '2':
                            optVerEncomendasDisponiveisEmpresa();
                            break;
                        case '3':
                            optRegistarDuracaoCusto();
                            break;
                        case 'S':
                            model.salvaEstado(model);
                            break;
                        default:
                            break;
                    }
                }
                break;
            default:
                break;

        }
        while (option != 'X') ;
    }

}

    public void optVerEncomendasDisponiveisEmpresa() {
        // ver da loja
    }

    public void optRegistarDuracaoCusto() {
        // same voluntario

    }

    public void optSinalizarEncomendaDisponivel() {

    }

public void optIndicarFila(){

} */
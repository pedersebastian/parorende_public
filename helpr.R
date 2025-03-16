fix_variable_name <- function(variable_name) {
  results <-
    map_chr(
      variable_name,
      function(x) {
        out <- switch(x,
          BehandlingHoeflighetRespektMedfoelelse_2 = c(
            "Hvordan ble pasienten møtt av intensivpersonalet",
            " med hensyn til høflighet, respekt og medfølelse?"
          ),
          SymptomSmerte_2 = c(
            "Hvor godt vurderte og behandlet intensivpersonalet ",
            "symptomene til pasienten med hensyn til smerte?"
          ),
          SymptomPustebesvaer_2 = c(
            "Hvor godt vurderte og behandlet intensivpersonalet ",
            "symptomene til pasienten med hensyn til pustebesvær?"
          ),
          SymptomUro_2 = c(
            "Hvor godt vurderte og behandlet intensivpersonalet ",
            "symptomene til pasienten med hensyn til uro?"
          ),
          BehandlingBesvarerBeho_2v = "Hvor godt viste intensivpersonalet interesse for dine behov?",
          BehandlingBesvarerStoette_2 = c(
            "Hvor god var den følelsesmessige støtten",
            "som du fikk av intensivpersonalet?"
          ),
          BehandlingSamarbeid_2 = c(
            "Hvordan samarbeidet intensivpersonalet som ivaretok",
            "og behandlet pasienten?"
          ),
          BehandlingBesvarerHoeflighetRespektMedfoelelse_2 = c(
            "Hvordan ble du møtt av intensivpersonalet",
            "med hensyn til høflighet, respekt og medfølelse?"
          ),
          SykepleierOmsorg_2 = "Hvor godt synes du sykepleierne ivaretok pasienten?",
          SykepleierKommunikasjon_2 = "Hvor ofte snakket sykepleierne med deg om pasientens tilstand?",
          LegeBehandling_2 = "Hvor godt synes du legene ivaretok pasienten?",
          AtmosfaerenIntensivAvd_2 = "Atmosfæren i intensivavdelingen var:",
          AtmosfaerenPaaroerenderom_2 = "Atmosfæren på pårørenderommet/venterommet var:",
          OmfangetAvBehandlingen_2 = c(
            "Hvor tilfreds var du med nivå eller omfang av",
            "pleie og behandling som pasienten fikk på intensivavdelingen?"
          ),
          LegeInformasjonFrekvens_2 = "Hvor ofte snakket legene med deg om pasientens tilstand?",
          SvarPaaSpoersmaal_2 = "Hvor villig var intensivpersonalet til å svare på dine spørsmål?",
          ForklaringForstaaelse_2 = "Hvor godt klarte intensivpersonalet å gi deg forklaringer som du forsto?",
          InformasjonsAerlighet_2 = c(
            "Hvor ærlig synes du informasjonen du fikk",
            "om tilstanden til pasienten var?"
          ),
          InformasjonOmForloep_2 = c(
            "Hvor godt ble du informert om hva som skjedde med pasienten",
            "og hvorfor ting ble gjort?"
          ),
          InformasjonsOverensstemmelse_2 = c(
            "Hvor stor overensstemmelse var det i informasjonen",
            "du fikk om tilstanden til pasienten?"
          ),
          BeslutningsInvolvering_2 = "Følte du deg involvert i beslutningsprosessen?",
          BeslutningsStoette_2 = "Følte du at du fikk støtte når beslutningene ble tatt?",
          BeslutningsKontroll_2 = "Følte du at du hadde innflytelse på den behandlingen \nsom ditt familiemedlem fikk?",
          BeslutningsTid_2 = c(
            "Når beslutninger skulle tas, hadde du tilstrekkelig med tid til ",
            "å uttrykke dine bekymringer og få besvart dine spørsmål?"
          ),
          LivsLengde_2 = "Hvilket utsagn beskriver best din oppfatning \nang. livet til pasienten:",
          LivssluttKomfor_2 = "Under de siste timene av livet til pasienten, hvilket utsagn
               beskriver best din oppfatning om hvordan han/hun hadde det:",
          LivssluttStoette_2 = "Under de siste timene før pasienten døde, hvordan \nfølte at du ble involvert beslutningsprosessen?",
          SumScoreSatisfactionCare = "Totalskår, omsorg",
          SumScoreSatisfactionDecision = "Totalskår, beslutningsmedvirkning",
          SumScoreAllQuestions = "Totalskår",
          MengdenAvHelsetjenester = "Hvor fornøyd er du med mengden eller nivået av helsetjenester ditt familiemedlem mottok i intensivavdelingen",
          DeltagelseIOmsorg = "Hvor fornøyd er du med din deltakelse i omsorgen for ditt kritisk syke familiemedlem?",
          MengdenAvHelsetjenester = "Noen mennesker ønsker at alt skal bli gjort for deres helseproblemer mens andre ønsker ikke at så mye skal gjøres.\n Hvor fornøyd er du med NIVÅET eller mengden av helsetjenester som ditt familiemedlem mottok i intensivavdelingen?",
          NA_character_
        )

        if (length(out) > 1) {
          out <- str_c(out, sep = "\n", collapse = "\n")
        }
        return(out)
      }
    )

  return(results)
}



get_labels <- function(var) {
  if (var %in% c(Del1[c(1:9, 11:13)], Del2[c(2:6)])) {
    # -1 = Velg verdi	1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	6 = Ikke aktuelt
    # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
    grtxt <- c("Fremragende", "Meget godt", "Godt", "Noenlunde", "Dårlig") # , '','Ikke svart')
  }
  if (var %in% c(Del1[10], Del2[1])) {
    #      -1:Velg verdi, 1:Svært ofte, 2: Ofte, 3: Av og til, 4: Sjelden, 5: Aldri, 6: Ikke relevant
    # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
    grtxt <- c("Svært ofte", "Ofte", "Av og til", "Sjelden", "Aldri") # , '','Ikke svart')
  }
  if (var %in% Del1[14:16]) {
    #-1 = Velg verdi	1 = Svært fornøyd	2 = Meget fornøyd	3 = Middels fornøyd	4 = Ganske misfornøyd
    # 5 = Svært misfornøyd	6 = Ikke relevant
    # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
    grtxt <- c(
      "Svært fornøyd", "Meget fornøyd", "Middels fornøyd",
      "Ganske misfornøyd", "Svært misfornøyd"
    ) # , '','Ikke svart')
  }

  if (var == "BeslutningsInvolvering") { # Del2[7]
    #-1:5

    grtxt <- c(
      "veldig utelatt", "noe utelatt", "verken eller", "noe involvert",
      "veldig involvert"
    ) # , '','Ikke svart') #paste0('Jeg følte meg ', )
  }
  if (var == "BeslutningsStoette") { # Del2[8]
    #-1:5

    grtxt <- c(
      "ikke støtte", "liten støtte", "en viss støtte",
      "støtte", "mye støtte"
    ) # , '','Ikke svart') #paste0('Jeg følte at jeg ', )
  }
  if (var == "BeslutningsKontroll") { # Del2[9]
    #-1:5

    grtxt <- c(
      "helt uten innflytelse", "liten innflytelse", "verken eller",
      "en viss innflytelse", "god innflytelse"
    ) # , '','Ikke svart')
  }
  if (var == "BeslutningsTid") { # Del2[10]
    #-1:2
    grtxt <- c("trengte mer tid", "tilstrekkelig med tid")
    # koder <- c(1,2,8,9)
  }
  if (var == "LivsLengde") { # Del2[11], #-1:5
    grtxt <- c(
      "unødvendig forlenget", "forlenget litt mer enn nødvendig",
      "passe", "forkortet litt mer enn nødvendig", "unødvendig forkortet"
    )
  }
  if (var == "LivssluttKomfor") { # Del2[12] #-1:5
    grtxt <- c(
      "ukomfortabelt", "noe ukomfortabelt", "stort sett \nkomfortabelt",
      "svært komfortabelt", "fullstendig komfortabelt"
    )
  }
  if (var == "LivssluttStoette") { # Del2[13] #-1:5

    grtxt <- c("veldig utelatt", "noe utelatt", "verken eller", "noe involvert", "veldig involvert")
  }
  return(grtxt)
}

fix_levels <- function(x, variable_name = cur_column()) {
  lbls <- get_labels(variable_name)
  lbls <- c(lbls, "", "Ikke svart")
  factor(x, levels = c(1:(length(lbls) - 2), 8:9), labels = lbls)
}



Del1 <- c(
  "BehandlingHoeflighetRespektMedfoelelse_2",
  "SymptomSmerte_2",
  "SymptomPustebesvaer_2",
  "SymptomUro_2",
  "BehandlingBesvarerBeho_2v",
  "BehandlingBesvarerStoette_2",
  "BehandlingSamarbeid_2",
  "BehandlingBesvarerHoeflighetRespektMedfoelelse_2",
  "SykepleierOmsorg_2",
  "SykepleierKommunikasjon_2",
  "LegeBehandling_2",
  "AtmosfaerenPaaroerenderom_2",
  "AtmosfaerenIntensivAvd_2",
  "OmfangetAvBehandlingen_2",
  "DeltagelseIOmsorg",
  "MengdenAvHelsetjenester"
)
Del2 <- c(
  "LegeInformasjonFrekvens_2",
  "SvarPaaSpoersmaal_2",
  "ForklaringForstaaelse_2",
  "InformasjonsAerlighet_2",
  "InformasjonOmForloep_2",
  "InformasjonsOverensstemmelse_2",
  "BeslutningsInvolvering_2",
  "BeslutningsStoette_2",
  "BeslutningsKontroll_2",
  "BeslutningsTid_2",
  "LivsLengde_2",
  "LivssluttKomfor_2",
  "LivssluttStoette_2"
)
##
# Lage nye navn
Del1Skaar <- paste0(Del1, "_score")
Del2Skaar <- paste0(Del2, "_score")
# Lage nye navn
Del1_text <- paste0(Del1, "_text")
Del2_text <- paste0(Del2, "_text")
######################

exec_case_when_custom <- function(x) {
  new_name <- paste0(x, "_text")

  case_when_special <- filter(case_whens_fun, variabelnavn_2 == {{ x }}) |>
    pull(fun) |>
    pluck(1)

  raw_data |>
    select(all_of(x)) |>
    mutate(
      !!new_name := case_when_special(!!sym(x)),
      !!x := if_else(!!sym(new_name) == "Ikke aktuelt", NA, !!sym(x))
    )
}



generate_case_when_function <- function(df, var_name) {
  # Filtrer for riktig variabel
  sub_df <- df[df$variabelnavn == var_name, ]

  # Bygg en liste med formler `value == x ~ "text"`
  case_conditions <- map2(
    sub_df$value, sub_df$text,
    ~ rlang::expr(x == !!.x ~ !!.y)
  )

  # Lag en case_when-funksjon
  case_expr <- expr(factor(case_when(!!!case_conditions, TRUE ~ NA_character_),
    levels = sub_df$text
  ))

  # Returnerer en funksjon som kan brukes i mutate()
  function(x) eval(case_expr, list(x = x))
}



get_letter <- function(n) {
  # Definer alfabetet
  alphabet <- LETTERS
  result <- ""

  # Sjekk at n er et positivt tall
  if (n < 1) {
    return("Ugyldig input. Skriv inn et positivt tall.")
  }

  # Konverter tall til bokstav(er)
  while (n > 0) {
    n <- n - 1 # Justering fordi alfabetet starter på 1, ikke 0
    remainder <- n %% 26
    result <- paste0(alphabet[remainder + 1], result)
    n <- n %/% 26
  }

  return(result)
}

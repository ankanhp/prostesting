load_input <- function(input) {
  #' Load an input data frame
  #'
  #' @param input A data frame.
  #' @return A data frame.
  
  
    
  input <- input %>%
    mutate(across(everything(), as.character)) %>% # Set all columns as character as default
    mutate_all(~ifelse(is_empty(.), NA, .)) %>% # Set all empty strings to NA
    transmute(Authorized = 'Open', # Provide guidance for all lines
              BOM.USG = extBomUsg,
              Configuration.ID = extBundleConfigurationId,
              Bundle.Line.Item.Number = NA_integer_, # No Bundle.Line.Item.Number equivalent for CPQ
              Bundled.Product.Desc = extBundledProductDesc,
              Base.Product.ID = extBundledProductId,
              Country = 'United States', # Hard-code country
              Customer.Segment = extCustomerSegment,
              Start = as.Date(extStart, format = '%m/%d/%Y'),
              Business.Model.Description = extBusinessModelDescription,
              Deal.Description = extDealDescription,
              End = as.Date(extEnd, format = '%m/%d/%Y'),
              Deal.ID = as.integer(dealId),
              District.Manager.Workforce.Full.Name = extDistMgr,
              Eclipse.Customer.Name = extCustomerName,
              Global.Business.Unit.Name = extGlobalBusinessUnitName,
              Miscellaneous.Charge.Code = extMiscChargeCode,
              Product.Family = extProductBaseCategoryDesc,
              Quantity = as.integer(qty),
              Requested.Discount.Additional.USD = as.numeric(extQuotedAdditionalDiscountUSD),
              COGS = as.numeric(extQuotedCostOfSalesUSD),
              Discount.Standard.USD = as.numeric(extQuotedStandardDiscountUSD),
              Currency.Code = currencyCode,
              Region = NA_character_, # Hard-code country
              ST.ID = as.integer(extStId),
              Unbundled.Product.Desc = extUnbundledProductDesc,
              SKU = productId,
              Unbundled.Product.Line.Desc = extUnbundledProductLineDesc,
              PL = extProductLine,
              Banded.Product.Flag = extBandedProductFlag,
              lineId = as.integer(lineId),
              LP = as.numeric(price),
              Requested.NP = LP - Discount.Standard.USD - Requested.Discount.Additional.USD) %>%
    mutate(Customer.Segment = recode(Customer.Segment,
                                     `CEM` = 'CEM',
                                     `Channels` = 'Channels',
                                     `CHNL` = 'Channels',
                                     `Corporate` = 'Corporate',
                                     `CORP` = 'Corporate',
                                     `Enterprise` = 'Enterprise',
                                     `Global` = 'Global',
                                     `PBLI` = 'PUBLIC SECTOR',
                                     `PUBLIC SECTOR` = 'PUBLIC SECTOR',
                                     `PUBLIC_SECTOR` = 'PUBLIC SECTOR',
                                     `Small Medium Business` = 'Small Medium Business',
                                     `SMB` = 'Small Medium Business',
                                     `Unidentified` = 'Unidentified',
                                     .default = 'Unidentified'), # TODO: Implement a comprehensive mapping
           Deal.Miscellaneous.Charge.Code.Deal.Type.Name = ifelse(exists('extMCDiscountType', .), extMCDiscountType, ''), # TODO: Add this field to the PSM
           Product.Configuration.Id = paste0(Deal.ID, '_', if_else(is.na(Configuration.ID), Base.Product.ID, Configuration.ID))) %>%
    select(sort(names(.)))

  return(input)
}

profile_input <- function(input) {
  #' Return metadata about an input
  #'
  #' @param input A data frame.
  #' @return `list(country, customer_segment, currency, cpq)`

  country <- case_when(identical(first(input$Country), 'United States') ~ 'US', identical(first(input$Region), 'EU') ~ 'EU')
  customer_segment <- first(input$Customer.Segment)
  currency <- ifelse(country == 'US', 'USD', first(input$Currency.Code))
  cpq <- 'lineId' %in% names(input)
  return(list(country, customer_segment, currency, cpq))
}

convert_currency_to_usd <- function(input, currency, cpq, country) {
  #' Re create lookup_eu_er and promo according to the deal currency
  #'
  #' @param input A dataframe.
  #' @param currency A value.
  #' @param cpq A boolean.
  #' @param country A country.
  #' @return `list(currency, input, lookup_eu_er, lookup_promo)`

  if (country == "US") {
    lookup_eu_er <- NULL
    lookup_promo <- NULL
    lookup_np_override <- NULL
  } else if (country == "EU") {

    lookup_eu_er <- bind_rows(
      lookup_eu_er,
      lookup_eu_er$date %>%
        unique() %>%
        as.data.frame() %>%
        mutate(
          currency.name = "US Dollars",
          currency.code = "USD",
          base.unit = "USD",
          value = 1
        ) %>%
        rename(date = 1) %>%
        select(everything(), date)
    )

    if (!cpq) {
      currency <-
        lookup_eu_er %>%
        filter((currency.name %>% str_to_sentence() %>% stringr::word(1)) == (currency %>% str_to_sentence() %>% stringr::word(1))) %>%
        pull(currency.code) %>%
        first()
    }

    if (difftime(Sys.Date(), max(lookup_eu_er$date), units = "days") %>% as.double() > 0) {

      lookup_eu_er <- lookup_eu_er %>%
        filter(
          date == max(date),
          currency.code == currency
        ) %>%
        mutate(value = if_else(base.unit == "USD", 1/value, value))

    } else {

      lookup_eu_er <- lookup_eu_er %>%
        filter(
          date %>% year() == Sys.Date() %>% year() & date %>% month() == Sys.Date() %>% month(),
          currency.code == currency
        ) %>%
        mutate(value = if_else(base.unit == "USD", 1/value, value))

    }

    if (cpq) {

      input <- input

      lookup_promo <- lookup_promo %>%
        mutate_at(
          vars(contains("Net.Price")),
          funs(. * lookup_eu_er$value)
        )

      lookup_np_override <- lookup_np_override %>%
        mutate_at(
          vars(contains(".NP.")),
          funs(. * lookup_eu_er$value)
        )

    } else if (currency != "USD") {
      input <- input %>%
        mutate_at(
          vars(c("COGS", "Discount.Standard.USD", "LP", "Requested.Discount.Additional.USD", "Requested.NP")),
          funs(. * lookup_eu_er$value)
        )
    }
  }
  
  return(c(currency, list(input, lookup_eu_er, lookup_promo, lookup_np_override)))
}

process_input <- function(input, country) {
  #' Process an input in preparation for the consolidation of its configurations
  #'
  #' @param input A data frame.
  #' @param country A country.
  #' @return A data frame with the columns needed for consolidation.

  label_configurations <- function(df, country) {
    #' Add columns: `CTO.or.BTO`, `Smart.or.Bom`, `Product.Configuration.Id`, `Configuration.Type`
    #'
    #' @param df A data frame.
    #' @param country A country.
    #' @return A data frame.
    #' @references https://github.azc.ext.hp.com/10XROI/Master-File/blob/master/label_configurations.R

    # Split into configurations with/without configuration ID and apply labeling rules
    if (country == 'US') {
      BTO_CUSTOM_ENDINGS <- c('EC', 'EP', 'PC', 'PP', 'UC', 'UP', 'US')
      BTO_CUSTOM_ENDING_LENGTH <- 2
      BTO_SMART_BUY_ENDINGS <- c('A8', 'AT', 'UT')
      TOP_VALUE_ENDINGS <- NULL # Top Value is the EU equivalent of Smart Buy
    } else if (country == 'EU') {
      BTO_CUSTOM_ENDINGS <- c('C', 'P', 'S')
      BTO_CUSTOM_ENDING_LENGTH <- 1
      BTO_SMART_BUY_ENDINGS <- NULL
      TOP_VALUE_ENDINGS <- 'T'
    }

    df_with_config_id <- df %>%
      filter(!is.na(Configuration.ID)) %>%
      mutate(CTO.or.BTO = 'CTO',
             Smart.or.Bom = if_else(str_sub(SKU, -2, -1) == 'AV', 'Normal', 'no_info'),
             Smart.or.Bom = if_else(str_sub(Base.Product.ID, -2, -1) == 'AV', 'Normal', Smart.or.Bom),
             CTO.or.BTO = if_else(str_sub(SKU, -BTO_CUSTOM_ENDING_LENGTH, -1) %in% BTO_CUSTOM_ENDINGS, 'BTO', CTO.or.BTO),
             Smart.or.Bom = if_else(str_sub(SKU, -BTO_CUSTOM_ENDING_LENGTH, -1) %in% BTO_CUSTOM_ENDINGS, 'Custom', Smart.or.Bom),
             CTO.or.BTO = if_else(str_sub(SKU, -1, -1) %in% TOP_VALUE_ENDINGS, 'BTO', CTO.or.BTO),
             Smart.or.Bom = if_else(str_sub(SKU, -1, -1) %in% TOP_VALUE_ENDINGS, 'Top Value', Smart.or.Bom))

    df_without_config_id <- df %>%
      filter(is.na(Configuration.ID)) %>%
      mutate(CTO.or.BTO = if_else(str_sub(SKU, -2, -1) == 'AV', 'CTO', 'BTO'),
             Smart.or.Bom = if_else(str_sub(SKU, -2, -1) == 'AV', 'Line Item', 'Normal'),
             Smart.or.Bom = if_else(str_sub(SKU, -2, -1) %in% BTO_SMART_BUY_ENDINGS, 'Smart Buy', Smart.or.Bom),
             Smart.or.Bom = if_else(str_sub(SKU, -1, -1) %in% TOP_VALUE_ENDINGS, 'Top Value', Smart.or.Bom),
             CTO.or.BTO = if_else(str_sub(SKU, -BTO_CUSTOM_ENDING_LENGTH, -1) %in% BTO_CUSTOM_ENDINGS, 'BTO', CTO.or.BTO),
             Smart.or.Bom = if_else(str_sub(SKU, -BTO_CUSTOM_ENDING_LENGTH, -1) %in% BTO_CUSTOM_ENDINGS, 'Custom', Smart.or.Bom))

    # Recombine
    df <- df_with_config_id %>% bind_rows(df_without_config_id)

    # Apply additional labeling rules
    # A configuration is a CTO normal if the base product is AV and the product is no_info
    cto_normal_candidates <- df %>%
      filter(CTO.or.BTO == 'CTO', Smart.or.Bom == 'Normal', SKU == Base.Product.ID) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>%
      mutate(Smart.or.Bom = if_else(CTO.or.BTO == 'CTO' & Smart.or.Bom == 'no_info' & Product.Configuration.Id %in% cto_normal_candidates, 'Normal', Smart.or.Bom),
             Smart.or.Bom = if_else(CTO.or.BTO == 'BTO' & Smart.or.Bom == 'Normal' & SKU != Base.Product.ID, 'Line Item', Smart.or.Bom),
             CTO.or.BTO = if_else(CTO.or.BTO == 'BTO' & Smart.or.Bom == 'Line Item' & SKU != Base.Product.ID, 'CTO', CTO.or.BTO))

    # Try to group some CTO Normals even if they do not have a configuration ID
    cto_normal_candidates <- df %>%
      filter(CTO.or.BTO == 'CTO', Smart.or.Bom == 'Line Item', !is.na(Bundle.Line.Item.Number), Base.Product.ID == SKU) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>% mutate(Smart.or.Bom = if_else(Product.Configuration.Id %in% cto_normal_candidates, 'Normal', Smart.or.Bom))

    # A configuration is a CTO BOM 3/5 if any SKU has a BOM USG of 3/5
    bom_3_candidates <- df %>% filter(BOM.USG == '3') %>% pull(Product.Configuration.Id) %>% unique()
    bom_5_candidates <- df %>% filter(BOM.USG == '5') %>% pull(Product.Configuration.Id) %>% unique()

    df <- df %>% mutate(CTO.or.BTO = if_else(Product.Configuration.Id %in% union(bom_3_candidates, bom_5_candidates), 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bom_3_candidates, 'Bom 3', Smart.or.Bom),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bom_5_candidates, 'Bom 5', Smart.or.Bom))

    # A configuration is a CTO BOM 3/5 if its configuration ID contains 3B/5B
    df <- df %>% mutate(CTO.or.BTO = if_else(grepl('3B', Configuration.ID), 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(grepl('3B', Configuration.ID), 'Bom 3', Smart.or.Bom),
                        CTO.or.BTO = if_else(grepl('5B', Configuration.ID), 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(grepl('5B', Configuration.ID), 'Bom 5', Smart.or.Bom))

    # BTO Customs/Smart Buys/Top Values are CTO BOM 3 if they contain other parts
    bom_3_candidates <- df %>%
      filter(!is.na(Configuration.ID), CTO.or.BTO == 'BTO', Smart.or.Bom %in% c('Custom', 'Smart Buy', 'Top Value')) %>%
      group_by(Product.Configuration.Id) %>%
      filter(n() > 1) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>% mutate(CTO.or.BTO = if_else(Product.Configuration.Id %in% bom_3_candidates, 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bom_3_candidates, 'Bom 3', Smart.or.Bom))

    # A configuration is a CTO BOM 3 if its base product ID ends with a particular character
    bom_3_candidates <- df %>%
      filter(!is.na(Configuration.ID)) %>%
      group_by(Product.Configuration.Id) %>%
      filter(sum(str_sub(Base.Product.ID, -1, -1) %in% c('A', 'W', '3', '4', '5', '6')) == n(), n() > 1) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>% mutate(CTO.or.BTO = if_else(Product.Configuration.Id %in% bom_3_candidates, 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bom_3_candidates, 'Bom 3', Smart.or.Bom))

    # A configuration is a BOM 3 soft bundle if all its parts are CTO no_info
    bom_3_candidates <- df %>%
      filter(!is.na(Configuration.ID)) %>%
      group_by(Product.Configuration.Id) %>%
      filter(sum(Smart.or.Bom == 'no_info') == n(), n() > 1) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>% mutate(CTO.or.BTO = if_else(Product.Configuration.Id %in% bom_3_candidates, 'CTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bom_3_candidates, 'Bom 3', Smart.or.Bom))

    # A configuration is a BTO Normal if it has a configuration ID but has only one line which is no_info
    bto_normal_candidates <- df %>%
      filter(!is.na(Configuration.ID), Smart.or.Bom == 'no_info') %>%
      group_by(Product.Configuration.Id) %>%
      filter(n() == 1) %>%
      pull(Product.Configuration.Id) %>%
      unique()

    df <- df %>% mutate(CTO.or.BTO = if_else(Product.Configuration.Id %in% bto_normal_candidates, 'BTO', CTO.or.BTO),
                        Smart.or.Bom = if_else(Product.Configuration.Id %in% bto_normal_candidates, 'Normal', Smart.or.Bom))

    # Ensure single-line configurations (BTOs and Line Items) are unique
    single_line_configs <- df %>%
      filter(CTO.or.BTO == 'BTO' | Smart.or.Bom == 'Line Item') %>%
      distinct(Product.Configuration.Id, .keep_all = T)

    df <- df %>%
      filter(!(CTO.or.BTO == 'BTO' | Smart.or.Bom == 'Line Item')) %>%
      bind_rows(single_line_configs) %>%
      mutate(Configuration.Type = paste0(CTO.or.BTO, ' ', Smart.or.Bom))

    return(df)
  }

  flag_in_scope <- function(df, country) {
    #' Add columns: `In.Scope`
    #'
    #' @param df A data frame.
    #' @param country A country.
    #' @return A data frame.

    if (country == 'US') {
      IN_SCOPE_PLS <- c('16', '2C', '5U', '5X', '67', '6U', '7F', '8J', '8N', '8W', '9F', '9H', '9R', 'AN', 'BO', 'BQ',
                        'DG', 'FF', 'G7', 'GA', 'GB', 'I0', 'I1', 'IK', 'IL', 'IQ', 'M0', 'M1', 'M2', 'MG', 'MN', 'MP',
                        'TA', 'TB', 'US', 'UT', 'UV')

      df <- df %>% mutate(In.Scope = Authorized == 'Open' & COGS > 0 & COGS < LP & PL %in% IN_SCOPE_PLS)
    } else if (country == 'EU') {
      IN_SCOPE_PLS <- c('16', '2C', '2G', '2H', '52', '5U', '5X', '6U', '7F', '8J', '8N', '8W', '9F', '9G', '9H', '9R',
                        '9T', 'AN', 'BO', 'BQ', 'CY', 'DG', 'EZ', 'FD', 'FF', 'FG', 'G7', 'GA', 'GB', 'I0', 'I1', 'IK',
                        'IL', 'IQ', 'M0', 'M1', 'M2', 'M4', 'MG', 'MN', 'MP', 'TA', 'TB', 'US', 'UT', 'UV')

      df <- df %>% mutate(In.Scope = Authorized == 'Open' & COGS > 0 & COGS < LP & PL %in% IN_SCOPE_PLS)
    }

    return(df)
  }

  add_processor_flag <- function(df, country) {
    #' Add columns: `Processor.Flag`
    #'
    #' @param df A data frame. Must have columns: `CTO.or.BTO`, `Smart.or.Bom`.
    #' @param table_path A string. The path of lookup table.
    #' @return A data frame with the added columns.

    if (country == 'US')
      return(df %>% mutate(Processor.Flag = NA))

    # CTO Normals
    cto_normals <- df %>%
      filter(CTO.or.BTO == 'CTO' & Smart.or.Bom == 'Normal') %>%
      group_by(Product.Configuration.Id) %>%
      mutate(Processor.Flag = any(str_detect(Unbundled.Product.Desc, 'Core'))) %>%
      ungroup()

    # CTO Line items
    cto_line_items <- df %>% filter(Smart.or.Bom == 'Line Item')

    # BTOs and Boms
    btos_and_boms <- df %>%
      filter(CTO.or.BTO == 'BTO' | Smart.or.Bom %in% c('Bom 3', 'Bom 5')) %>%
      left_join(processor_btos, by = c('Base.Product.ID' = 'SKU'))

    # Consolidate
    df <- bind_rows(cto_normals, cto_line_items, btos_and_boms)

    return(df)
  }

  add_country_code <- function(df, country) {
    #' Add columns: `Eclipse.Customer.Country.Code`
    #'
    #' @param df A data frame.
    #' @return A data frame with the added columns.
    #'
    if (country == 'US') {
      df <- df %>% mutate(Eclipse.Customer.Country.Code = 'US')
    } else if (country == 'EU') {
      df <- df %>%
        left_join(lookup_country_code, by = 'Country') %>%
        left_join(lookup_country_code, by = c('Country' = 'country_code')) %>%
        mutate(Eclipse.Customer.Country.Code = if_else(country_code %>% is.na(), Country, country_code)) %>%
        select(-c(Proxy_model_code.x, Proxy_model_code.y, country_code, Country.y))
    }

    return(df)
  }

  extract_ps_features <- function(quotes, country) {
    #' Add columns: `Form.Factor`, `Model`, `Series`, `Product.Type`
    #'
    #' @details
    #' Feature extraction is split into three sections and then concatenated:
    #' 1. CTO Normals: Features are extracted from `Unbundled.Product.Desc`.
    #' 2. BTOs and BOMs: Features are extracted from lookup tables.
    #' 3. Other: Features are not extracted.
    #'
    #' @param quotes A data frame.
    #' @param country A country.
    #' @return A data frame.
    #' @references https://github.azc.ext.hp.com/10XROI/Master-File/blob/master/extract_ps_features.R

    # Temporarily ignore warnings in this function
    op <- options(warn = -1)
    on.exit(options(op))

    FORM_FACTOR_PATTERN <- '3\\-in\\-1|[Aa][Ii][Oo]|Chromebook|Chromebox|Clamshell|Convertible|Detachable|Desktop Mini|DM|Mini|MT|Notebook|SFF|Small Form Factor|Tablet|TC|Tower|TWR|UltraBook|UltraSlim|UltraThin|USFF|[Xx]360'

    quotes <- quotes %>% mutate(SKU.Base = str_sub(SKU, 1, 7))

    # 1. CTO Normals
    cto_normals <- quotes %>% filter(CTO.or.BTO == 'CTO', Smart.or.Bom == 'Normal')

    lookup_commercial_desktops <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'Commercial Desktops', str_starts(Unbundled.Product.Desc, 'BU|HP|ProOne')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = str_extract(Unbundled.Product.Desc, FORM_FACTOR_PATTERN),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = str_match(Model, paste0('(?:BU|HP|ProOne)\\s*(.*?)\\s*(?:PC|', Form.Factor, ')'))[,2],
             Model = str_remove_all(Model, 'BRZL|Brazil|RCTO|EON|IDS|PON|PCI|NT|T|FHD|GPU|RFID'),
             Model = str_remove(Model, '\\d{2}W$'),
             Model = gsub('(G\\d).*', '\\1', Model),
             Model = str_squish(str_remove_all(Model, 'EliteDesk|EliteOne|Elite|ProDesk|ProOne|EDK|PDK|BU|HP|ProOne|\\d{2}W'))) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_commercial_notebooks <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'Commercial Notebooks', str_starts(Unbundled.Product.Desc, 'BU')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = case_when(str_detect(Unbundled.Product.Desc, '[Xx]360') ~ 'x360', str_detect(Unbundled.Product.Desc, 'TAB|[Xx]2') ~ 'Detachable', TRUE ~ 'Notebook'),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = word(Model, -2, -1),
             Model = str_squish(str_remove_all(Model, '\\d+GB|x\\d|BU|^(i|I)\\S*')),
             Model = if_else(str_starts(Model, 'G\\d|HC'), word(Unbundled.Product.Desc, -3, -1), str_replace(Model, 'fWWAN|fWAN|NFC', ' ')),
             Model = str_squish(Model),
             Model = str_squish(str_remove(Model, '\\d{1,2}G[Bb]')),
             Model = if_else(str_starts(Model, 'fHD|FHD|IDS|RCTO|fCAM'), word(Model, -1), Model),
             Model = gsub('(G\\d).*', '\\1', Model),
             Model = str_squish(str_replace(Model, 'G', ' G')),
             Model = str_replace(Model, '1011|11', 'CB11'),
             Model = str_replace(Model, '1012|12', 'CB12'),
             Model = str_replace(Model, '1013|13', 'CB13'),
             Model = str_replace(Model, '1014|14', 'CB14'),
             Model = if_else(str_starts(Model, '32G'), str_squish(str_sub(str_remove(Model, 'wKBD'), -6, -1)), Model),
             Model = if_else(str_detect(Model, '6083'), '830 G6', Model),
             Model = if_else(str_starts(Model, '64G'), str_squish(str_sub(str_remove(Model, 'wKBD'), -8, -1)), Model),
             Model = if_else(str_starts(Model, '6C'), str_squish(str_remove(str_replace(Model, '6C', 'C'), 'TABwPKBD|Tablet|TAB')), Model),
             Model = if_else(Model == 'B14AG5', 'CB14A G5', Model),
             Model = if_else(Model == 'CB128GeMMC 210G2TAB', '210 G2', Model),
             Model = if_else(str_detect(Model, 'EliteG4|Elite G4'), 'x2 G4', Model),
             Model = if_else(str_starts(Model, 'FHDPVCYTS|fSCR|fCam|nCam'), str_remove_all(Model, 'FHDPVCYTS|fSCR|fCam|NCAM'), Model),
             Model = if_else(str_starts(Model, 'HD'), str_replace(str_sub(Model, -6, -1), 'C', ''), Model),
             Model = str_squish(str_remove_all(Model, 'HC|EE|TAB|OSR|wTKBD|MC')),
             Model = if_else(str_starts(Model, 'IDS|m5|m7'), NA_character_, Model),
             Model = if_else(str_starts(Model, 'RCTO'), str_sub(Model, -6, -1), Model),
             Model = if_else(str_starts(Model, 'UMA '), str_sub(Model, -7, -1), Model),
             Model = if_else(str_starts(Model, 'UMAi'), NA_character_, Model),
             Model = if_else(str_starts(Model, 'X5'), str_sub(str_replace(Model, 'wKB', ''), -5, -1), Model),
             Model = if_else(str_starts(Model, 'LCB|N3350|nCAM|Pent'), str_sub(Model, -7, -1), Model),
             Model = if_else(str_detect(Model, '640 G5'), '640 G5', Model),
             Model = if_else(str_detect(Model, 'S612G2'), 'CB12 G2', Model),
             Model = if_else(str_starts(Model, '0|BL|60'), str_remove(Model, '0|BL|60'), Model)) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_mobile_workstations <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'Mobile Workstations', str_starts(Unbundled.Product.Desc, 'BU')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = case_when(str_detect(Unbundled.Product.Desc, '[Xx]360') ~ 'x360', str_detect(Unbundled.Product.Desc, 'TAB|[Xx]2') ~ 'Detachable', TRUE ~ 'Notebook'),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = word(Unbundled.Product.Desc, -2, -1),
             Model = str_remove(Model, '^(i|I)\\S*'),
             Model = str_squish(str_replace(Model, 'Studio', ' Studio')),
             Model = if_else(str_detect(Model, '(?<!^)Studio'), word(Model, -1), Model),
             Model = if_else(str_starts(Model, 'P1000|T2000'), str_sub(Model, -4, -1), Model),
             Model = str_squish(str_replace(Model, 'G', ' G')),
             Model = if_else(str_starts(Model, '\\d{2}'), str_c('Z', Model), Model),
             Model = if_else(str_detect(Model, 'Studiox360'), 'Studio G5', Model),
             Model = if_else(str_starts(Model, 'BU|DSC|RCTO|RTX|T1000|Z3000|Z32|Z4000|Z64|fHDC|UHD'), str_squish(str_sub(Model, -6, -1)), Model),
             Model = str_squish(str_remove(Model, 'BT|Cam|CC|DC|ED|GB|B|am|\\-')),
             Model = if_else(str_starts(Model, '\\d{2}'), str_c('Z', Model), Model)) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_psg_retail_solutions <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'PSG Retail Solutions', str_starts(Unbundled.Product.Desc, 'BU')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = str_extract(Unbundled.Product.Desc, FORM_FACTOR_PATTERN),
             Form.Factor = if_else(str_detect(Unbundled.Product.Desc, 'TAB'), 'Detachable', Form.Factor),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = str_match(Model, paste0('BU\\s*(.*?)\\s*', Form.Factor))[,2],
             Model = str_remove_all(Model, 'RCTO|IDS|T|AMS'),
             Model = if_else(is.na(Model), str_remove_all(word(Unbundled.Product.Desc, -3, -1), 'UMA|BU|IDS|\\dGB|\\d\\dGB'), Model),
             Model = str_squish(Model),
             Model = if_else(str_detect(Model, 'EngageGo'), 'Engage Go', Model),
             Model = if_else(str_detect(Model, 'fWWAN'), word(str_replace(Model, 'fWWAN', ' '), -1), Model),
             Model = str_remove(Model, 'EliteDesk|TAB|RSTAB')) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_thin_clients <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'Thin Clients', str_starts(Unbundled.Product.Desc, 'BU')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = if_else(str_detect(Unbundled.Product.Line.Desc, 'Mobile'), 'Mobile Thin Client', 'Thin Client'),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = if_else(Form.Factor == 'Thin Client', str_extract(Model, 't\\d{3}'), word(Model, -4, -1)),
             Model = str_remove_all(Model, 'IDS|UMA|Cel'),
             Model = if_else(str_starts(Model, '3865|A8|R3'), str_squish(str_sub(Model, -4, -1)), Model),
             Model = str_squish(str_remove(Model, '3865U'))) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_workstations <- cto_normals %>%
      filter(SKU == Base.Product.ID, Product.Family == 'Workstations', str_starts(Unbundled.Product.Desc, 'BU|HP')) %>%
      distinct(Product.Configuration.Id, .keep_all = T) %>%
      mutate(Form.Factor = if_else(str_detect(Unbundled.Product.Desc, 'Z4|Z6|Z8'), 'TWR', str_extract(Unbundled.Product.Desc, FORM_FACTOR_PATTERN)),
             Form.Factor = case_when(!is.na(Form.Factor) ~ Form.Factor, str_detect(Unbundled.Product.Desc, 'Mini') ~ 'MT', str_detect(Unbundled.Product.Desc, 'TOWER|Tower') ~ 'TWR', TRUE ~ Form.Factor),
             Model = if_else(is.na(Form.Factor), NA_character_, Unbundled.Product.Desc),
             Model = str_match(Unbundled.Product.Desc, '(?:BU|HP)\\s*(.*?)\\s*(?:WKS|Workstation)')[,2],
             Model = str_squish(str_remove_all(Model, 'RCTO|Tower|AMS|SFF|TWR|Mini')),
             Model = gsub('(G\\d).*', '\\1', Model),
             Model = str_squish(str_remove(Model, 'EliteDesk|LL|IDS|Core-X|MT'))) %>%
      select(Product.Configuration.Id, Form.Factor, Model)

    lookup_cto_normals <- bind_rows(lookup_commercial_desktops,
                                    lookup_commercial_notebooks,
                                    lookup_mobile_workstations,
                                    lookup_psg_retail_solutions,
                                    lookup_thin_clients,
                                    lookup_workstations) %>%
      mutate(Form.Factor = case_when(Form.Factor == 'Notebook' ~ 'Clamshell',
                                     Form.Factor == 'Tower' ~ 'TWR',
                                     str_detect(Form.Factor, '[Xx]360') ~ 'x360'),
             Model = str_squish(str_replace(Model, 'G', ' G')),
             Model = if_else(str_detect(Model, '^\\w+ G\\d+$'), Model, NA_character_),
             Model = if_else(Model == 'CB12 G1', 'x21012 G1', Model),
             Model = if_else(Model == 'CB12 G2', 'x21012 G2', Model),
             Model = if_else(Model == 'CB12 G1', 'x21012 G1', Model),
             Model = if_else(Model == 'Z14 G2', 'CB14 G2', Model),
             Series = str_squish(str_remove_all(Model, 'G\\d'))) %>%
      select(Product.Configuration.Id, Form.Factor, Model, Series)

    cto_normals <- cto_normals %>%
      left_join(lookup_cto_normals, by = 'Product.Configuration.Id') %>%
      mutate(Form.Factor = case_when(!is.na(Form.Factor) ~ Form.Factor,
                                     str_detect(Bundled.Product.Desc, FORM_FACTOR_PATTERN) ~ str_extract(Bundled.Product.Desc, FORM_FACTOR_PATTERN),
                                     str_detect(Bundled.Product.Desc, 'Tower|TOWER|Z[468]') ~ 'TWR',
                                     str_detect(Bundled.Product.Desc, 'TAB|[Xx2]') ~ 'Detachable',
                                     str_detect(Bundled.Product.Desc, '[Xx]360') ~ 'x360',
                                     str_detect(Bundled.Product.Desc, 'TC') ~ 'TC',
                                     TRUE ~ Form.Factor))

    # 2. BTOs and BOMs
    btos_and_boms <- quotes %>%
      filter(CTO.or.BTO == 'BTO' | Smart.or.Bom %in% c('Bom 3', 'Bom 5')) %>%
      left_join(lookup_btos_and_boms, by = 'SKU.Base')

    # 3. Other
    other <- quotes %>% filter(CTO.or.BTO == 'CTO', !Smart.or.Bom %in% c('Bom 3', 'Bom 5', 'Normal'))

    # Concatenate
    quotes <- bind_rows(cto_normals, btos_and_boms, other) %>%
      mutate(Form.Factor = case_when(is.na(Form.Factor) | !str_detect(Form.Factor, FORM_FACTOR_PATTERN) ~ 'Other',
                                     str_detect(Form.Factor, '[Aa][Ii][Oo]') ~ 'AIO',
                                     TRUE ~ Form.Factor),
             Global.Business.Unit.Name = case_when(str_detect(Global.Business.Unit.Name, 'Commercial Display') ~ 'Commercial Displays, Accy, & 3PO',
                                                   str_detect(Global.Business.Unit.Name, 'Consumer Display') ~ 'Consumer Displays & Accessories',
                                                   str_detect(Global.Business.Unit.Name, 'PS Commercial Serv') ~ 'PS Commercial Services',
                                                   TRUE ~ Global.Business.Unit.Name),
             Product.Type = case_when(str_detect(Global.Business.Unit.Name, 'Consumer') ~ 'Remove from dashboard',
                                      Global.Business.Unit.Name == 'Commercial Compute' & str_detect(Product.Family, 'Accessories') ~ 'Accessories',
                                      Global.Business.Unit.Name == 'Commercial Compute' & str_detect(Product.Family,  'Other') ~ 'Other',
                                      Global.Business.Unit.Name == 'Commercial Compute' & !str_detect(Product.Family, 'Other|Accessories') ~ 'Unit',
                                      Global.Business.Unit.Name == 'Commercial Displays, Accy, & 3PO' & str_detect(Product.Family, 'Display') ~ 'Display',
                                      Global.Business.Unit.Name == 'Commercial Displays, Accy, & 3PO' & str_detect(Product.Family, 'Acccesories|Workstations|PSG Retail Solutions|Desktops|Thin') ~ 'Accessories',
                                      Global.Business.Unit.Name == 'Commercial Displays, Accy, & 3PO' & str_detect(Product.Family, 'Services') ~ 'Services',
                                      Global.Business.Unit.Name == 'Commercial Displays, Accy, & 3PO' & str_detect(Product.Family, 'PSG Other Commercial') ~ 'Other',
                                      Global.Business.Unit.Name == 'Commercial Displays, Accy, & 3PO' & str_detect(Product.Family, 'Commercial Third Party Options') ~ 'Remove from dashboard',
                                      Global.Business.Unit.Name == 'Immersive Computing' & Product.Family == 'Immersive Computing' ~ 'Unit',
                                      Global.Business.Unit.Name == 'PS Commercial Services' ~ 'Services',
                                      Global.Business.Unit.Name == 'PSG Other' ~ 'Services',
                                      Global.Business.Unit.Name == 'PSG Other Commercial' & str_detect(Product.Family, 'Other') ~ 'Other',
                                      Global.Business.Unit.Name == 'PSG Other Commercial' & !str_detect(Product.Family, 'Other') ~ 'Accessories',
                                      Global.Business.Unit.Name == 'PSG Retail Solutions' & !str_detect(Product.Family, 'Services|Accessories|Other') ~ 'Unit',
                                      Global.Business.Unit.Name == 'PSG Retail Solutions' & str_detect(Product.Family, 'Services') ~ 'Services',
                                      Global.Business.Unit.Name == 'PSG Retail Solutions' & str_detect(Product.Family, 'Other') ~ 'Other',
                                      Global.Business.Unit.Name == 'PSG Retail Solutions' & str_detect(Product.Family, 'Accessories') ~ 'Accessories',
                                      TRUE ~ 'Other'))

    return(quotes)
  }

  join_contra_coefficients <- function(df, country) {
    #' Add columns: `Contra.c1`, `Contra.c2`
    #'
    #' @param df A data frame.
    #' @param country A country.
    #' @return A data frame with the added columns.

    df <- df %>% mutate(Discount.Standard = Discount.Standard.USD / LP,
                        NDP = LP - Discount.Standard.USD,
                        Flag.Indirect = as.integer(Business.Model.Description %in% c('Global Indirect', 'GSI Global Indirect', 'Indirect/Partner Dir', 'IPG Indirect')),
                        Flag.Smart.Buy = as.integer(Smart.or.Bom == 'Smart Buy'))

    if (country == 'US') {
      df <- df %>%
        left_join(lookup_contra, by = 'PL') %>%
        mutate(Contra.c1 = case_when(Flag.Indirect == 0 ~ 0,
                                     Flag.Smart.Buy == 0 ~ NDP_Indirect_Distribution,
                                     Flag.Smart.Buy == 1 ~ NDP_Indirect_Smartbuy),
               Contra.c2 = case_when(Flag.Indirect == 0 ~ 0,
                                     Flag.Smart.Buy == 0 ~ NaSND_Indirect_Distribution,
                                     Flag.Smart.Buy == 1 ~ NaSND_Indirect_Smartbuy)) %>%
        select(-NDP_Indirect_Distribution, -NDP_Indirect_Smartbuy, -NaSND_Indirect_Distribution, -NaSND_Indirect_Smartbuy) %>%
        replace_na(list(Contra.c1 = 0, Contra.c2 = 0))
    } else {
      df <- df %>% mutate(Contra.c1 = 0, Contra.c2 = 0)
    }

    return(df)
  }

  join_empowerment_gmp <- function(df, country) {
    #' Add columns: `Empowerment.GMp`
    #'
    #' @param df A data frame.
    #' @param country A country.
    #' @return A data frame with the added column.
    #'
    #' @details
    #' The US empowerment lookup tables indicate GM% empowerment levels after contra is applied. Since the GM% grid in
    #' the optimization consists of GM% before contra is applied, we need to convert the GM% empowerment levels from
    #' after contra is applied to before contra is applied to derive the lower bound of the GM% grid.
    #'
    #' First we calculate the net price that corresponds to the empowerment GM% after contra. Then we calculate the GM%
    #' (the empowerment GM% before contra) that corresponds to this net price.
    #'
    #' Below is the derivation of this net price.
    #'
    #' Define the following variables.
    #'   E: Empowerment GM% after contra
    #'   p: Net price
    #'   NDP: Net dealer price
    #'   c1: Contra as % of NDP
    #'   c2: Contra as % of NP
    #'   K: COGS
    #'
    #' Define the following functions.
    #'   GM%A(p) = (p - c1 * NDP - c2 * p - K) / (p - c1 * NDP - c2 * p): The GM% after contra corresponding to p
    #'   GM%B(p) = (p - K) / p: The GM% before contra corresponding to p
    #'
    #' To calculate the net price that corresponds to the empowerment GM% after contra, we set GM%A(p) = E and solve for p.
    #'   GM%A(p) = (p - c1 * NDP - c2 * p - K) / (p - c1 * NDP - c2 * p) = E
    #'   p - c1 * NDP - c2 * p - K = E(p - c1 * NDP - c2 * p)
    #'   p(1 - c2) - c1 * NDP - K = p(E(1 - c2)) - E(c1 * NDP)
    #'
    #' Subtract p(1 - c2) - E(c1 * NDP) from both sides.
    #'   -c1 * NDP - K + E(c1 * NDP) = p(E - 1)(1 - c2)
    #'   p = ((E - 1) * c1 * NDP - K) / ((E - 1)(1 - c2))
    #'   p = ((1 - E) * c1 * NDP + K) / ((1 - E)(1 - c2))

    extract_series_empowerment <- function(df) {
      #' Add columns: `Series.Clean`
      #'
      #' @details
      #' We need to make sure to not accidentally extract storage capacity instead of series (e.g. 128/256/512/1024).
      #'
      #' @param A data frame.
      #' @return A data frame.

      df <- df %>% mutate(Series.Empowerment = case_when(
        str_detect(Bundled.Product.Desc, 'Engage Go') | str_detect(Series, 'Engage Go') ~ 'Engage Go',
        str_detect(Bundled.Product.Desc, '[Zz]8') | str_detect(Series, 'Z8|z8') ~ 'Z8',
        str_detect(Bundled.Product.Desc, 'Studio') | str_detect(Series, 'Studio') ~ 'Studio/Create',
        str_detect(Bundled.Product.Desc, 'Engage Flex') | str_detect(Series, 'Engage Flex') ~ 'Engage Flex Pro/Pro-C',
        str_detect(Bundled.Product.Desc, 'Engage One') | str_detect(Series, 'Engage One') ~ 'Engage One/Engage One Prime',
        str_detect(Bundled.Product.Desc, 'MP9') | str_detect(Series, 'MP9') ~ 'mp9/Engage Edge',
        str_detect(Bundled.Product.Desc, 'Z6') | str_detect(Series, 'Z6') ~ 'Z6',
        str_detect(Bundled.Product.Desc, 'Z4') | str_detect(Series, 'Z4') ~ 'Z4/Z4R',
        str_detect(Bundled.Product.Desc, 'Z2 Mini') | str_detect(Series, 'Z2 Mini') ~ 'Z2 Mini',
        str_detect(Bundled.Product.Desc, 'Z[12]') | str_detect(Series, 'Z1|Z2') ~ 'Z1/Z2',
        str_detect(Bundled.Product.Desc, '[Xx]2') | str_detect(Series, 'x2|X2') ~ 'X2 Series Hybrid',
        str_detect(Bundled.Product.Desc, 't310') | str_detect(Series, 't310') ~ 't310',
        str_detect(Bundled.Product.Desc, 'RP9|RP5|rp9|rp5') | str_detect(Series, 'RP9|RP5|rp9|rp5') ~ 'rp5/rp9',
        PL == '6U' & (str_detect(Bundled.Product.Desc, '(?!256)2\\d{2}') | str_detect(Series, '(?!256)2\\d{2}')) ~ 'Edu/200 Series NB',
        PL == '6U' & (str_detect(Bundled.Product.Desc, '4\\d{2}') | str_detect(Series, '4\\d{2}')) ~ '400 Series NB',
        PL == '6U' & (str_detect(Bundled.Product.Desc, '6\\d{2}') | str_detect(Series, '6\\d{2}')) ~ '600 Series NB',
        PL == 'AN' & (str_detect(Bundled.Product.Desc, '8\\d{2}|(?!1024)1\\d{3}') | str_detect(Series, '8\\d{2}|(?!1024)1\\d{3}')) ~ '800/1000 Series NB',
        PL %in% c('5U', 'DG') & (str_detect(Bundled.Product.Desc, '(?!256)2\\d{2}') | str_detect(Series, '(?!256)2\\d{2}')) ~ '200 Series DT',
        PL %in% c('5U', 'DG') & (str_detect(Bundled.Product.Desc, '[46]\\d{2}') | str_detect(Series, '[46]\\d{2}')) ~ '400/600 Series DT',
        PL %in% c('7F', 'GA') & (str_detect(Bundled.Product.Desc, '8\\d{2}') | str_detect(Series, '8\\d{2}')) ~ '800 Series DT',
        str_detect(Bundled.Product.Desc, 't4|t5|t6|t7') | str_detect(Series,'t4|t5|t6|t7') ~ 't4/5/6/7',
        str_detect(Unbundled.Product.Desc, '[Ff]ury') | str_detect(Bundled.Product.Desc,'[Ff]ury') ~ 'Fury',
        str_detect(Unbundled.Product.Desc, '[Ss]tudio|[Cc]reate') | str_detect(Bundled.Product.Desc, '[Ss]tudio|[Cc]reate') ~ 'Studio/Create',
        PL == 'TA' & (str_detect(Unbundled.Product.Desc, 'ZB') | str_detect(Bundled.Product.Desc, 'ZB')) ~ 'Fury',
        TRUE ~ 'Other'))

      return(df)
    }

    if (country == 'US') {
      df <- df %>%
        extract_series_empowerment() %>%
        left_join(lookup_empowerment_pl, by = 'PL') %>%
        left_join(lookup_empowerment_pl_series, by = c('PL', 'Series.Empowerment' = 'Series')) %>%
        mutate(Empowerment.GMp = case_when(!is.na(Empowerment.PL.Series) ~ Empowerment.PL.Series,
                                           !is.na(Empowerment.PL) ~ Empowerment.PL,
                                           TRUE ~ 0),
               p = ((1 - Empowerment.GMp) * Contra.c1 * NDP + COGS) / ((1 - Empowerment.GMp) * (1 - Contra.c2)),
               Empowerment.Discount.USD = LP - p)
    } else if (country == 'EU') {

      empowerment_levels_parts <- function(premodel, emp_lev){
        #' Add column `EL_disc`
        #'
        #' @param premodel A data frame
        #' @param emp_lev A data frame of Empowerment levels
        #' @return A data frame with the added column

        # Prepare quotes2 data frame to merge the empowerment levels
        series.600.countries <- lookup_empowerment %>%
          filter(PL2.b == "6U", !is.na(comment_1)) %>%
          select(Eclipse.Customer.Country.Code) %>%
          unique() %>%
          pull()

        premodel <-
          premodel %>% mutate(
            PL2.b = PL,
            row = 1:nrow(premodel),
            Business.Model.Description3 = factor(
              case_when(
                Business.Model.Description %in% c("Global Direct", "GSI Global Direct", "HP Direct", "Value Direct") ~ "Direct",
                Business.Model.Description %in% c("Volume Channel", "Global Indirect", "GSI Global Indirect", "Value Indirect") ~ "Indirect",
                TRUE ~ as.character(Business.Model.Description)
              )
            ),
            # comment_1 is an indicator of special case, check the lookup table
            comment_1 = ifelse(
              PL2.b == "6U" & (Eclipse.Customer.Country.Code %in% series.600.countries) &
                (Series %>% str_sub(1, 3) %>% as.numeric()) >= 600 &
                (Series %>% str_sub(1, 3) %>% as.numeric()) < 700,
              "(600 series only)",
              NA
            )
          )

        #Create table
        df_EL <-
          premodel %>%
          select(row, Eclipse.Customer.Country.Code, Base.Product.ID, Product.Configuration.Id, PL2.b, SKU, Business.Model.Description3, Series, LP, COGS, comment_1) %>% # Relevant Columns from premodel
          left_join(
            emp_lev,
            by = c("Eclipse.Customer.Country.Code", "PL2.b", "Business.Model.Description3", "comment_1")
          ) %>% # Merge
          mutate(
            EL_disc = 1 - (COGS/(LP*(1-EL))),
            EL_disc = ifelse(EL_disc < 0, 0, EL_disc)
          ) %>% # Compute Max Discount `max_disc`. If it is negative, compute 0
          select(row, EL_disc, EL) %>%
          unique()

        #Merge
        #* Merge into df_limits
        premodel <-
          premodel %>%
          merge(
            df_EL,
            by = c("row"),
            all.x = T, all.y = F
          ) %>%
          mutate(
            EL_disc = replace_na(EL_disc, 1)
          )

        return(premodel)
      }

      # Identify for each SKU, what's the empowerment level. Then, calculate the max disc usd for each part
      df <- df %>%
        empowerment_levels_parts(lookup_empowerment) %>%
        mutate(EL_disc = pmin(EL_disc, 1),
               EL_disc = pmax(EL_disc, 0),
               Empowerment.Discount.USD = EL_disc * LP)
    }

    return(df)
  }

  join_promo_gmp <- function(df, country) {
    #' Add columns: `Promo.Discount`
    #'
    #' @param df A data frame.
    #' @return A data frame with the added columns.
    #'
    if (country == 'EU') {
      #'Set discount lower boundaries

      #Number of row
      quotes <- df %>% mutate(row = 1:nrow(df))

      #* Create Lower Bound
      df_promo <- quotes %>%
        mutate(List.Price = LP) %>%
        select(row, Eclipse.Customer.Country.Code, Configuration.ID, Base.Product.ID, SKU, Product.Configuration.Id, Start, End, List.Price, CTO.or.BTO) %>% # Relevant Columns from premodel
        left_join(
          lookup_promo,
          by = c("Eclipse.Customer.Country.Code", "SKU")
        ) %>% # Merge
        arrange(Product.Configuration.Id, Start.y) %>%
        group_by(row) %>%
        filter(Start.x <= End.y & Start.x >= Start.y) # Consider current available Promos

      if (nrow(df_promo) != 0) {
        # Search the best (max) Price among `Net.Price`s Promos. Additionally, "FI" applies 5%.
        df_promo_floor <- df_promo %>%
          filter(Lever.Promo.Floor.Net.Price == min(Lever.Promo.Floor.Net.Price, na.rm = TRUE))

        df_promo_typical <- df_promo %>%
          filter(Lever.Promo.Typical.Net.Price == min(Lever.Promo.Typical.Net.Price, na.rm = TRUE)) %>%
          select(row, Lever.Promo.Typical.Net.Price)

        df_promo_expert <- df_promo %>%
          filter(Lever.Promo.Expert.Net.Price == min(Lever.Promo.Expert.Net.Price, na.rm = TRUE)) %>%
          select(row, Lever.Promo.Expert.Net.Price)

        df_promo <- df_promo_floor %>%
          left_join(df_promo_typical) %>%
          left_join(df_promo_expert) %>%
          mutate(
            best.Floor.Net.Price = case_when(
              Eclipse.Customer.Country.Code == "FI" ~ min(Lever.Promo.Floor.Net.Price, na.rm = TRUE) * .95,
              TRUE ~ min(Lever.Promo.Floor.Net.Price, na.rm = TRUE)
            ),
            best.Typical.Net.Price = case_when(
              Eclipse.Customer.Country.Code == "FI" ~ min(Lever.Promo.Typical.Net.Price, na.rm = TRUE) * .95,
              TRUE ~ min(Lever.Promo.Typical.Net.Price, na.rm = TRUE)
            ),
            best.Expert.Net.Price = case_when(
              Eclipse.Customer.Country.Code == "FI" ~ min(Lever.Promo.Expert.Net.Price, na.rm = TRUE) * .95,
              TRUE ~ min(Lever.Promo.Expert.Net.Price, na.rm = TRUE)
            )
          )

        if (nrow(df_promo) != 0) {
          df_promo <- df_promo %>%
            mutate(
              Floor.Discount = 1 - best.Floor.Net.Price/List.Price,
              Typical.Discount = 1 - best.Typical.Net.Price/List.Price,
              Expert.Discount = 1 - best.Expert.Net.Price/List.Price,
              best.Floor.Net.Price = case_when(
                List.Price < best.Floor.Net.Price ~ List.Price, # If `best.Net.Price` Promo is more expensive than the Deal `List.Price => List.Price
                CTO.or.BTO == "CTO" ~ List.Price * (1 - Floor.Discount), # If CTO => apply %discount to the Deal List.Price
                TRUE ~ best.Floor.Net.Price
              ),
              best.Typical.Net.Price = case_when(
                List.Price < best.Typical.Net.Price ~ List.Price,
                CTO.or.BTO == "CTO" ~ List.Price * (1 - Typical.Discount),
                TRUE ~ best.Typical.Net.Price
              ),
              best.Expert.Net.Price = case_when(
                List.Price < best.Expert.Net.Price ~ List.Price,
                CTO.or.BTO == "CTO" ~ List.Price * (1 - Expert.Discount),
                TRUE ~ best.Expert.Net.Price
              )
            ) %>%
            distinct(row, .keep_all = TRUE) %>% # Remove repeated `Product.Configuration.Id`
            ungroup() %>%
            group_by(Product.Configuration.Id) %>%
            summarise(best.Floor.Net.Price = sum(best.Floor.Net.Price, na.rm = T),
                      best.Typical.Net.Price = sum(best.Typical.Net.Price, na.rm = T),
                      best.Expert.Net.Price = sum(best.Expert.Net.Price, na.rm = T),
                      List.Price = sum(List.Price, na.rm = T)) %>%
            mutate(Promo.Floor.Discount = 1-best.Floor.Net.Price/List.Price,
                   Promo.Typical.Discount = 1-best.Typical.Net.Price/List.Price,
                   Promo.Expert.Discount = 1-best.Expert.Net.Price/List.Price) %>% # Compute % Discount
            select(Product.Configuration.Id, Promo.Floor.Discount, Promo.Typical.Discount, Promo.Expert.Discount) # keep relevant variables

          #Merge
          df <- merge(df, df_promo, by = "Product.Configuration.Id", all.x = T, all.y = F) %>%
            replace_na(list(Promo.Floor.Discount = 0, Promo.Typical.Discount = 0, Promo.Expert.Discount = 0))
        }
      }
      n_df_promo <- nrow(df_promo)
    } else {
      n_df_promo <- 0
    }

    if (n_df_promo == 0)
      df <- df %>% mutate(Promo.Floor.Discount = 0,
                          Promo.Typical.Discount = 0,
                          Promo.Expert.Discount = 0)

    df <- df %>% arrange(Bundle.Line.Item.Number)

    return(df)
  }

  df <- input %>%
    label_configurations(country) %>%
    flag_in_scope(country) %>%
    add_processor_flag(country) %>%
    add_country_code(country) %>%
    extract_ps_features(country) %>%
    join_contra_coefficients(country) %>%
    join_empowerment_gmp(country) %>%
    join_promo_gmp(country) %>%
    group_by(Deal.ID) %>%
    mutate(Nprod = n()) %>%
    ungroup() %>%
    select(sort(names(.)))

  return(df)

}

consolidate_configurations <- function(df) {
  #' Consolidate configurations
  #'
  #' @details
  #' In a PSM, headers are the first line of multi-line configurations and have Bundle.Line.Item.Number == 0.
  #' If a header exists, only the header has the correct quantity and all other lines in the configuration have a
  #' quantity of 1.
  #' After extracting the correct quantity from the header, we must drop the header.
  #'
  #' @param df A data frame. Must have the columns needed for consolidation.
  #' @return A data frame.

  df <- df %>%
    group_by(Product.Configuration.Id) %>%
    mutate(Quantity = max(Quantity)) %>% # Extract the correct quantity from the header if applicable
    filter(is.na(Bundle.Line.Item.Number) | Bundle.Line.Item.Number != 0) %>% # Filter out PSM headers
    arrange(desc(SKU == Base.Product.ID), desc(LP)) %>% # Move base products to the top, break ties with LP
    summarise(
      Authorized = first(Authorized),
      Base.Product.ID = first(Base.Product.ID),
      Bundled.Product.Desc = first(Bundled.Product.Desc),
      Business.Model.Description = first(Business.Model.Description),
      COGS = sum(COGS),
      Configuration.ID = first(Configuration.ID),
      Configuration.Type = first(Configuration.Type),
      Contra.LP = sum(Contra.c2 * LP),
      Contra.NDP = sum(Contra.c1 * NDP),
      Country = first(Country),
      CTO.or.BTO = first(CTO.or.BTO),
      Customer.Segment = first(Customer.Segment),
      Deal.Description = first(Deal.Description),
      Deal.ID = first(Deal.ID),
      Deal.Miscellaneous.Charge.Code.Deal.Type.Name = first(Deal.Miscellaneous.Charge.Code.Deal.Type.Name),
      District.Manager.Workforce.Full.Name = first(District.Manager.Workforce.Full.Name),
      Eclipse.Customer.Country.Code = first(Eclipse.Customer.Country.Code),
      End = first(End),
      Empowerment.Discount.USD = sum(Empowerment.Discount.USD),
      Flag.Indirect = first(Flag.Indirect),
      Flag.Smart.Buy = first(Flag.Smart.Buy),
      In.Scope = all(In.Scope),
      LP = sum(LP),
      Miscellaneous.Charge.Code = first(Miscellaneous.Charge.Code),
      Nprod = first(Nprod),
      Number.of.parts = n(),
      PL = first(PL),
      Processor.Flag = first(Processor.Flag),
      Product.Family = first(Product.Family),
      Product.Type = first(Product.Type),
      Promo.Floor.Discount = first(Promo.Floor.Discount),
      Promo.Typical.Discount = first(Promo.Typical.Discount),
      Promo.Expert.Discount = first(Promo.Expert.Discount),
      Quantity = first(Quantity),
      Requested.Discount.Additional.USD = sum(Requested.Discount.Additional.USD),
      Discount.Standard.USD = sum(Discount.Standard.USD),
      Requested.NP = sum(Requested.NP),
      SKU = first(SKU),
      Smart.or.Bom = first(Smart.or.Bom),
      ST.ID = first(ST.ID),
      Start = first(Start),
      Series = first(Series),
      Unbundled.Product.Desc = first(Unbundled.Product.Desc),
      Unbundled.Product.Line.Desc = first(Unbundled.Product.Line.Desc)
    ) %>%
    select(sort(names(.)))

  return(df)
}

process_configurations <- function(df, country) {
  #' Process configurations
  #'
  #' @param df A data frame. Must be consolidated by configuration.
  #' @param country A country.
  #' @return `c(df_open, df_no_guidance)`

  add_lookup_tables <- function(df, country) {
    #' Merge with lookup tables
    #'
    #' @param df A data frame after general processing.
    #' @param country A country.
    #' @return A data frame with added features from lookup tables.

    if (country == 'US') {
      df <- df %>%
        left_join(lookup_customer, by = 'ST.ID') %>%
        replace_na(list(Flag.Ever.Had.Claims = 0, Flag.Ever.Had.Quotes = 0, Flag.Had.Quote.In.Window = 0, New = 1, Active = 0)) %>%
        left_join(lookup_customer_sku, by = c('ST.ID', 'SKU')) %>%
        replace_na(list(Flag.Ever.Had.Claims.SKU = 0, Flag.Ever.Had.Quotes.SKU = 0, Lagged.BD.Qty = 0))
    } else if (country == 'EU') {
      df <- df %>%
        left_join(lookup_lagged, by = c('ST.ID', 'SKU')) %>%
        mutate(Lagged.BD.Qty = Most.Recent.BD.Qty) %>%
        left_join(lookup_country_regions, by = c('Eclipse.Customer.Country.Code')) %>%
        mutate(Country.Region = factor(Country.Region)) %>%
        left_join(lookup_claim, by = 'ST.ID') %>%
        replace_na(list(Flag.Ever.Had.Claims = 0)) %>%
        mutate(New = factor(1 - as.numeric(as.character(Flag.Ever.Had.Claims)))) %>%
        left_join(lookup_display_rate, by = 'ST.ID') %>%
        left_join(lookup_district_manager_wr, by = 'District.Manager.Workforce.Full.Name') %>%
        left_join(lookup_extra_customer_segment, by = 'ST.ID')

      # Update Customer.Segment
      df <- df %>%
        mutate(
          Customer.Segment = case_when(
            Customer.Segment == 'Channels' ~ 'Small Medium Business',
            Customer.Segment == 'Corporate' ~ 'Global',
            Customer.Segment == 'Enterprise' ~ 'CEM',
            Customer.Segment == 'Unidentified' & Customer.Segment.2 == 'CEM' ~ 'CEM',
            Customer.Segment == 'Unidentified' & Customer.Segment.2 == 'GLOBAL' ~ 'Global',
            Customer.Segment == 'Unidentified' & Customer.Segment.2 == 'PUBLIC_SECTOR' ~ 'PUBLIC SECTOR',
            Customer.Segment == 'Unidentified' & Customer.Segment.2 == 'SMB' ~ 'Small Medium Business',
            TRUE ~ Customer.Segment
          )
        ) %>%
        select(-Customer.Segment.2)
    }

    return(df)
  }

  COVID_START_DATE <- as.Date('2020-03-23')

  df <- df %>%
    add_lookup_tables(country) %>%
    mutate(
      Discount.Standard = Discount.Standard.USD / LP,
      Discount.Standard.GMp = 1 - COGS / (LP * (1 - Discount.Standard)),
      Duration = as.numeric(End - Start),
      Empowerment.Discount = Empowerment.Discount.USD / LP,
      Empowerment.NP = (1 - Empowerment.Discount) * LP,
      Empowerment.GMp = 1 - COGS / Empowerment.NP,
      Flag.COVID = as.integer(End > COVID_START_DATE),
      Gross.Revenue = Quantity * LP,
      Deal.Gross.Revenue = sum(Gross.Revenue),
      Nconfig = n(),
      Prop.COVID = if_else(Flag.COVID == 1, as.numeric((End - pmax(COVID_START_DATE, Start)) / Duration), 0),
      Requested.Discount.Additional = Requested.Discount.Additional.USD / LP,
      Requested.Discount = 1 - Requested.NP / LP,
      Requested.Contra = Contra.NDP + (1 - Requested.Discount) * Contra.LP,
      Requested.Net.Revenue = Quantity * Requested.NP,
    ) %>%
    select(sort(names(.)))

  return(df)
}

split_by_scope <- function(df) {
  #' Split into two data frames: In-scope and out-of-scope configurations
  #'
  #' @param df A data frame.
  #' @return list(df_in_scope, df_out_of_scope)

  df_in_scope <- df %>% filter(In.Scope)
  if (nrow(df_in_scope) == 0)
    df_in_scope <- NULL

  df_out_of_scope <- df %>% filter(!In.Scope)
  if (nrow(df_out_of_scope) == 0)
    df_out_of_scope <- NULL

  return(list(df_in_scope, df_out_of_scope))
}

add_limits <- function(df, country) {
  #' Add columns: `Async.Floor.GMp`, `Async.Expert.GMp`, `Sync.Floor.GMp`, `Sync.Typical.GMp`, `Sync.Expert.GMp`
  #'
  #' @param df A data frame.
  #' @param country A country.
  #' @param customer_segment A customer segment.
  #' @return A data frame with the added columns.

  join_historical_gmp <- function(df, country) {
    #' Add historical GMp by greedily looking for smallest possible segment like Country, PL, Customer.Segment ...
    #'
    #' @param df A data frame.
    #' @param lookup_historical_full A list of 7 objects
    #' @return A data frame with the added historical GMp

    if (country == 'US') {
      original_cols <- colnames(df)

      for (i in length(lookup_segmentation_tables):1) {
        join_cols <- unlist(strsplit(names(lookup_segmentation_tables)[[i]], ','))
        if (i == length(lookup_segmentation_tables))
          df <- df %>% left_join(lookup_segmentation_tables[[i]], by = join_cols)
        else
          df[is.na(df$gmp0), ] <- df[is.na(df$gmp0), original_cols] %>% left_join(lookup_segmentation_tables[[i]], by = join_cols)
        if (!sum(is.na(df$gmp0)))
          break
      }

      df <- df %>% select(-n)

      return(df)
    } else if (country == 'EU') {
      df_full <- df
      df <- data.table(df_full)

      depth <- length(lookup_historical_full)

      lookup_historical_full <- lapply(lookup_historical_full, function(x) x[, !"Count"])
      allcols <- colnames(lookup_historical_full[[depth]])
      discols <- allcols[str_detect(allcols,"^gmp\\d+$")]
      grpcols <- allcols[allcols %in% colnames(df)]
      df <- df[, ..grpcols]
      for (i in depth:1) {
        joincols <- strsplit(names(lookup_historical_full)[i],"\\|")[[1]]
        lookup_historical <- lookup_historical_full[[i]]
        if (i == depth)
          df <- merge(df, lookup_historical, by=joincols, all.x=TRUE, na.rm=TRUE, sort=F)
        else
          df[is.na(gmp50),] <- merge(df[is.na(gmp50),][,! ..discols], lookup_historical, by=joincols, all.x=T, na.rm=T, sort=F)
        if (!sum(is.na(df$gmp50)))
          break
      }
      df_full[, discols] <- df[, ..discols]
      setDF(df_full)
      return(df_full)
    }
  }

  join_levers <- function(df, levers) {
    #' Add columns: `Lever.Discount.Min`, `Lever.Discount.Max`
    #'
    #' @param df A data frame.
    #' @param levers A data frame, levers lookup table.
    #' @return A data frame with the added columns.

    df <- df %>%
      left_join(
        levers,
        by = c("Eclipse.Customer.Country.Code", "Business.Model.Description3", "PL2")
      ) %>%
      mutate(
        Lever.Floor.Max.Discount = 1 - (COGS / (List.Price * (1 - Lever.Floor.Min.GMp))),
        Lever.Typical.Min.Discount = 1 - (COGS / (List.Price * (1 - Lever.Typical.Max.GMp)))
      )

    return(df)
  }

  join_margin_override <- function(df, lookup_margin_override) {
    #' Add columns: `Override.GMp`
    #'
    #' @param df A data frame.
    #' @param lookup_margin_override A data frame, margin override lookup table.
    #' @return A data frame with the added columns.

    lookup_margin_override_PL <- lookup_margin_override %>%
      filter(
        !PL2 %>% is.na(),
        df$Start >= Start & df$Start <= End
      ) %>%
      select(-c(Business.Model.Description3, SKU, Start, End))

    lookup_margin_override_SKU <- lookup_margin_override %>%
      filter(
        PL2 %>% is.na(),
        df$Start >= Start & df$Start <= End
      ) %>%
      select(-c(ST.ID, PL2, Start, End))

    df <- df %>%
      merge(lookup_margin_override_PL, by = c('Eclipse.Customer.Country.Code', 'ST.ID', 'PL2'), all.x =  TRUE) %>%
      merge(lookup_margin_override_SKU, by = c('Eclipse.Customer.Country.Code', 'Business.Model.Description3', 'SKU'), all.x =  TRUE) %>%
      mutate(
        Lever.Floor.Mrg.Ovr.GMp = if_else(Lever.Floor.Mrg.Ovr.GMp.x %>% is.na(), Lever.Floor.Mrg.Ovr.GMp.y, Lever.Floor.Mrg.Ovr.GMp.x),
        Lever.Typical.Mrg.Ovr.GMp = if_else(Lever.Typical.Mrg.Ovr.GMp.x %>% is.na(), Lever.Typical.Mrg.Ovr.GMp.y, Lever.Typical.Mrg.Ovr.GMp.x),
        Lever.Expert.Mrg.Ovr.GMp = if_else(Lever.Expert.Mrg.Ovr.GMp.x %>% is.na(), Lever.Expert.Mrg.Ovr.GMp.y, Lever.Expert.Mrg.Ovr.GMp.x),
      ) %>%
      select(-c(Lever.Floor.Mrg.Ovr.GMp.x, Lever.Typical.Mrg.Ovr.GMp.x, Lever.Expert.Mrg.Ovr.GMp.x, Lever.Floor.Mrg.Ovr.GMp.y, Lever.Typical.Mrg.Ovr.GMp.y, Lever.Expert.Mrg.Ovr.GMp.y))

    return(df)
  }

  join_np_override <- function(df, lookup_np_override) {
    #' Add columns: `Override.GMp`
    #'
    #' @param df A data frame.
    #' @param lookup_np_override A data frame, np override lookup table.
    #' @return A data frame with the added columns.

    lookup_np_override <- lookup_np_override %>%
      filter(df$Start >= Start & df$Start <= End) %>%
      select(-c(Start, End))

    df <- df %>%
      merge(lookup_np_override, by = c('Eclipse.Customer.Country.Code', 'Business.Model.Description3', 'SKU'), all.x =  TRUE) %>%
      mutate_at(
        vars(contains(".NP.Ovr.GMp")),
        funs(1 - COGS/.)
      )

    return(df)
  }

  consolidate_limits <- function(df) {
    #' Set max_disc and min_disc based on historical percentiles, Promo and Empowerment
    #'
    #' @param df A data frame
    #' @return  A data frame with consolidated min_disc and max_disc

    # Discount
    df <- df %>%
      mutate(
        Discount.Min = pmax(Discount.Standard, Promo.Floor.Discount, disc85, Lever.Typical.Min.Discount, na.rm = TRUE),
        Discount.Max = pmin(Empowerment.Discount, disc20, Lever.Floor.Max.Discount, na.rm = TRUE),
        Discount.Min = pmax(Discount.Min, 0, na.rm = TRUE),
        Discount.Min = pmin(Discount.Min, 1, na.rm = TRUE),
        Discount.Max = pmax(Discount.Max, 0, na.rm = TRUE),
        Discount.Max = pmin(Discount.Max, 1, na.rm = TRUE),
        Discount.Max = case_when(
          (Promo.Floor.Discount > disc85 & Promo.Floor.Discount > Lever.Typical.Min.Discount) &
            Discount.Min > Discount.Max ~ Discount.Min,
          TRUE ~ Discount.Max
        ),
        Discount.Min = case_when(
          (Promo.Floor.Discount < disc85 & Promo.Floor.Discount < Lever.Typical.Min.Discount) &
            Discount.Min > Discount.Max ~ Discount.Max,
          TRUE ~ Discount.Min
        ),
        # GMp
        GMp.Min = 1 - COGS/(List.Price*(1-Discount.Max)),
        GMp.Max = 1 - COGS/(List.Price*(1-Discount.Min)),
        GMp.Min = pmax(GMp.Min, 0, na.rm = TRUE),
        GMp.Max = pmax(GMp.Max, 0, na.rm = TRUE),
        GMp.Min = ceiling(100*round(GMp.Min, 5))/100,
        GMp.Max = floor(100*round(GMp.Max, 5))/100,
        GMp.Min = case_when(
          GMp.Max < GMp.Min ~ GMp.Max,
          TRUE ~ GMp.Min
        ),
        Promo.Floor.GMp = 1 - COGS/(List.Price*(1-Promo.Floor.Discount)),
        Promo.Typical.GMp = 1 - COGS/(List.Price*(1-Promo.Typical.Discount)),
        Promo.Expert.GMp = 1 - COGS/(List.Price*(1-Promo.Expert.Discount)),
        Upper.Boundary.a = pmin(Lever.Typical.Max.GMp, gmp85, Promo.Typical.GMp, na.rm = TRUE),
        Upper.Boundary.b = pmin(Lever.Expert.Max.GMp, gmp90, Promo.Expert.GMp, na.rm = TRUE),
        Async.Floor.GMp = case_when(
          (is.na(Lever.Floor.Mrg.Ovr.GMp) & is.na(Lever.Floor.NP.Ovr.GMp)) ~ GMp.Min,
          (!is.na(Lever.Floor.Mrg.Ovr.GMp) & is.na(Lever.Floor.NP.Ovr.GMp)) ~ Lever.Floor.Mrg.Ovr.GMp,
          TRUE ~ Lever.Floor.NP.Ovr.GMp
        ),
        Async.Expert.GMp = case_when(
          (is.na(Lever.Floor.Mrg.Ovr.GMp) & is.na(Lever.Floor.NP.Ovr.GMp)) ~ GMp.Max,
          (!is.na(Lever.Floor.Mrg.Ovr.GMp) & is.na(Lever.Floor.NP.Ovr.GMp)) ~ Lever.Floor.Mrg.Ovr.GMp,
          TRUE ~ Lever.Floor.NP.Ovr.GMp
        ),
        Sync.Floor.GMp = Async.Floor.GMp,
        Sync.Typical.GMp = case_when(
          (is.na(Lever.Typical.Mrg.Ovr.GMp) & is.na(Lever.Typical.NP.Ovr.GMp)) ~ pmax(Upper.Boundary.a, Lever.Typical.Min.GMp, na.rm = TRUE) %>% pmin(Promo.Typical.GMp, na.rm = TRUE),
          (!is.na(Lever.Typical.Mrg.Ovr.GMp) & is.na(Lever.Typical.NP.Ovr.GMp)) ~ Lever.Typical.Mrg.Ovr.GMp,
          TRUE ~ Lever.Typical.NP.Ovr.GMp
        ),
        Sync.Expert.GMp = case_when(
          (is.na(Lever.Expert.Mrg.Ovr.GMp) & is.na(Lever.Expert.NP.Ovr.GMp)) ~ pmax(Upper.Boundary.b, Lever.Expert.Min.GMp, na.rm = TRUE) %>% pmin(Promo.Expert.GMp, na.rm = TRUE),
          (!is.na(Lever.Expert.Mrg.Ovr.GMp) & is.na(Lever.Expert.NP.Ovr.GMp)) ~ Lever.Expert.Mrg.Ovr.GMp,
          TRUE ~ Lever.Expert.NP.Ovr.GMp
        ),
        Sync.Typical.GMp = if_else(Sync.Typical.GMp < 0 , 0, Sync.Typical.GMp),
        Sync.Expert.GMp = if_else(Sync.Expert.GMp < 0, 0, Sync.Expert.GMp),
        Async.Expert.GMp = if_else(Sync.Typical.GMp < Async.Expert.GMp, Sync.Typical.GMp, Async.Expert.GMp),
        Async.Floor.GMp = if_else(Async.Floor.GMp > Async.Expert.GMp, Async.Expert.GMp, Async.Floor.GMp)
      )

    return(df)
  }

  add_country_code_for_model <- function(df) {
    #' Replace column: `Eclipse.Customer.Country.Code`
    #'
    #' @param df A data frame.
    #' @return A data frame with the replaced columns.

    df <- df %>%
      left_join(lookup_country_code, by = 'Country') %>%
      left_join(lookup_country_code, by = c('Country' = 'country_code')) %>%
      mutate(
        Eclipse.Customer.Country.Code = if_else(is.na(Proxy_model_code.x), Proxy_model_code.y, Proxy_model_code.x)
      ) %>%
      select(-c(Proxy_model_code.x, Proxy_model_code.y, country_code, Country.y))

  }

  if (is.null(df))
    return()

  if (country == 'US') {
    df <- df %>%
      join_historical_gmp(country) %>%
      mutate(Async.Floor.GMp = pmin(Discount.Standard.GMp, pmax(gmp25, Empowerment.GMp)),
             Async.Expert.GMp = pmin(Discount.Standard.GMp, pmax(gmp75, Async.Floor.GMp)),
             Sync.Floor.GMp = Async.Floor.GMp,
             Sync.Typical.GMp = Async.Expert.GMp,
             Sync.Expert.GMp = pmin(Discount.Standard.GMp, pmax(gmp80, Sync.Typical.GMp)))
  } else if (country == 'EU') {

    Net.Rev.Buckets <- list("XS"=0, "S"=844, "M" = 3025, "L"=8436, "XL"=25111, "DMS"=125400)
    df$Net.Rev.Buckets <- cut(df$Requested.Net.Revenue, breaks=c(Net.Rev.Buckets, Inf), labels=names(Net.Rev.Buckets))
    df <- df %>%
      mutate(
        List.Price = LP,
        PL2 = PL,
        Flag.Top.Value = if_else(Smart.or.Bom == "Top Value", 1, 0),
        Business.Model.Description2 = recode(Business.Model.Description,
                                             `Global Direct` = 'Direct',
                                             `GSI Global Direct` = 'Direct',
                                             `HP Direct` = 'Direct',
                                             `Value Direct` = 'Direct',
                                             `Global Indirect` = 'Indirect',
                                             `GSI Global Indirect` = 'Indirect',
                                             `Value Indirect` = 'Indirect'),
        Business.Model.Description3 = factor(
          case_when(
            Business.Model.Description %in% c("Global Direct", "GSI Global Direct", "HP Direct", "Value Direct") ~ "Direct",
            Business.Model.Description %in% c("Volume Channel", "Global Indirect", "GSI Global Indirect", "Value Indirect") ~ "Indirect",
            TRUE ~ as.character(Business.Model.Description)
          )
        )
      ) %>%
      join_historical_gmp(country) %>%
      replace_na(list(
        gmp10 = 0,
        gmp20 = 0,
        gmp25 = 0,
        gmp75 = 1,
        gmp80 = 1,
        gmp85 = 1,
        gmp90 = 1
      )) %>%
      mutate(
        disc10 = 1 - COGS/(List.Price*(1-gmp10)),
        disc20 = 1 - COGS/(List.Price*(1-gmp20)),
        disc25 = 1 - COGS/(List.Price*(1-gmp25)),
        disc50 = 1 - COGS/(List.Price*(1-gmp50)),
        disc75 = 1 - COGS/(List.Price*(1-gmp75)),
        disc80 = 1 - COGS/(List.Price*(1-gmp80)),
        disc85 = 1 - COGS/(List.Price*(1-gmp85)),
        disc90 = 1 - COGS/(List.Price*(1-gmp90))
      ) %>%
      join_levers(lookup_levers) %>%
      join_margin_override(lookup_margin_override) %>%
      join_np_override(lookup_np_override) %>%
      consolidate_limits() %>%
      add_country_code_for_model()
  }

  return(df)
}

create_grid <- function(df) {
  #' Return a "stacked" data frame with a grid for each configuration over `GMp` with constraints on `GMp` and `Discount`
  #'
  #' @param df A data frame.
  #' @return A stacked data frame.
  #' @example
  #' Product.Configuration.Id  GMp                     Discount ...
  #'                   ABC123  0.1                         0.15
  #'                   ABC123 0.11                         0.13
  #'                   ABC123 0.12                         0.11
  #'                      ...  ...                          ...
  #'                   DEF456  0.4                         0.20
  #'                   DEF456 0.41                         0.19

  if (is.null(df))
    return()

  df_grid <- df %>%
    mutate(GMp = map2(Async.Floor.GMp, Async.Expert.GMp, seq, .01)) %>% # Define a grid of GMp per configuration
    unnest(GMp) %>% # Duplicate rows across each GMp step
    mutate(Discount = 1 + COGS / ((GMp - 1) * LP),
           NP = LP * (1 - Discount),
           AVG.Quoted.Discount.USD = LP * Discount,
           Contra = Contra.NDP + (1 - Discount) * Contra.LP)

  return(df_grid)
}

predict_grid <- function(df_grid, country, customer_segment) {
  #' Return a data frame with predictions
  #'
  #' @param df A data frame.
  #' @return a data frame with predictions

  create_design_matrix <- function(df_grid, country, customer_segment, model_type = NULL) {
    #' Create a design matrix
    #'
    #' @param df A data frame.
    #' @param country A country.
    #' @param customer_segment A customer segment.
    #' @param model_type Either 'Consumption' or 'Attainment'.
    #' @return A design matrix for model input.d

    if (country == 'US') {
      if (customer_segment != 'PUBLIC SECTOR') {
        if (model_type == 'Consumption') {
          design_matrix <- df_grid %>%
            transmute(
              NP.Bd = NP,
              Discount = Discount,
              Quantity = Quantity,
              Gross.Revenue = Gross.Revenue,
              Customer.Segment.Channels = as.integer(Customer.Segment == 'Channels'),
              Customer.Segment.Corporate = as.integer(Customer.Segment == 'Corporate'),
              Customer.Segment.Enterprise = as.integer(Customer.Segment == 'Enterprise'),
              Customer.Segment.Small.Medium.Business = as.integer(Customer.Segment == 'Small.Medium.Business'),
              Customer.Segment.Unidentified = as.integer(Customer.Segment == 'Unidentified'),
              Flag.COVID = Flag.COVID,
              Flag.Ever.Had.Quotes = Flag.Ever.Had.Quotes,
              Flag.Ever.Had.Claims = Flag.Ever.Had.Claims,
              New = New,
              Active = Active,
              Global = as.integer(Miscellaneous.Charge.Code %in% c('72G', '72M')),
              Deal.Type.CSAT = as.integer(str_detect(Deal.Description, fixed('Sat', ignore_case = T))),
              PL5Xf = as.integer(PL == '5X'),
              PL7Ff = as.integer(PL == '7F'),
              PL9Ff = as.integer(PL == '9F'),
              PLANf = as.integer(PL == 'AN'),
              PLBOf = as.integer(PL == 'BO'),
              PLDGf = as.integer(PL == 'DG'),
              PLGAf = as.integer(PL == 'GA'),
              PLIQf = as.integer(PL == 'IQ'),
              PLMGf = as.integer(PL == 'MG'),
              PLMPf = as.integer(PL == 'MP'),
              Smart.or.Bom.Custom = as.integer(Smart.or.Bom == 'Custom'),
              Smart.or.Bom.BTO.Normal = as.integer(Configuration.Type == 'BTO Normal'),
              Smart.or.Bom.Smart.Buy = as.integer(Smart.or.Bom == 'Smart Buy'),
              Smart.or.Bom.Line.Item = as.integer(Smart.or.Bom == 'Line Item'),
              cat.Commercial.Accessories = as.integer(Product.Family == 'Commercial Accessories'),
              cat.Commercial.Desktops = as.integer(Product.Family == 'Commercial Desktops'),
              cat.Commercial.Displays.L5 = as.integer(Product.Family == 'Commercial Displays (L5)'),
              cat.Commercial.Mobility = as.integer(Product.Family == 'Commercial Mobility'),
              cat.Commercial.Notebooks = as.integer(Product.Family == 'Commercial Notebooks'),
              cat.CommercialThird.Party = as.integer(Product.Family == 'Commercial Third Party Options'),
              cat.Consumer.Accessories = as.integer(Product.Family == 'Consumer Accessories '),
              cat.Consumer.Displays = as.integer(Product.Family == 'Consumer Displays'),
              cat.Consumer.Notebooks = as.integer(Product.Family == 'Consumer Notebooks'),
              cat.Immersive.Computing = as.integer(Product.Family == 'Immersive Computing'),
              cat.Mobile.Workstations = as.integer(Product.Family == 'Mobile Workstations'),
              cat.PS.Commercial.Contractual.Services = as.integer(Product.Family == 'PS Commercial Contractual Services'),
              cat.PS.Commercial.Support.Services = as.integer(Product.Family == 'PS Commercial Support Services'),
              cat.PS.Consumer.Transactional.Services = as.integer(Product.Family == 'PS Consumer Transactional Services'),
              cat.PSG.Other.Commercial = as.integer(Product.Family == 'PSG Other Commercial'),
              cat.PSG.Other.Consumer = as.integer(Product.Family == 'PSG Other Consumer'),
              cat.PSG.Retail.Solutions = as.integer(Product.Family == 'PSG Retail Solutions'),
              cat.Thin.Clients = as.integer(Product.Family == 'Thin Clients'),
              cat.Workstations = as.integer(Product.Family == 'Workstations'),
              cat.Accessories = as.integer(Product.Type == 'Accessories'),
              cat.Display = as.integer(Product.Type == 'Display'),
              cat.Other = as.integer(Product.Type == 'Other'),
              cat.Remove.from.dashboard = as.integer(Product.Type == 'Remove from dashboard'),
              cat.Services = as.integer(Product.Type == 'Services'),
              cat.Unit = as.integer(Product.Type == 'Unit')
            ) %>%
            as.matrix()
        } else if (model_type == 'Attainment') {
          design_matrix <- df_grid %>%
            group_by(Deal.ID, SKU) %>%
            mutate(
              Gross.Revenue.SKU = sum(Gross.Revenue),
              NP.BD.Gross.Revenue.SKU = sum(NP * Gross.Revenue)
            ) %>%
            group_by(Deal.ID, PL) %>%
            mutate(
              Gross.Revenue.PL = sum(Gross.Revenue),
              NP.BD.Gross.Revenue.PL = sum(NP * Gross.Revenue),
              Cannibalization.PL.NP = (NP.BD.Gross.Revenue.PL - NP.BD.Gross.Revenue.SKU) / (Gross.Revenue.PL - Gross.Revenue.SKU)
            ) %>%
            ungroup() %>%
            replace_na(list(Cannibalization.PL.NP = 0)) %>%
            transmute(
              Discount = Discount,
              Smart.or.Bom.Line.Item = factor(as.integer(Smart.or.Bom == 'Line Item')),
              Smart.or.Bom.Custom = factor(as.integer(Smart.or.Bom == 'Custom')),
              Customer.Segment.Corporate = factor(as.integer(Customer.Segment == 'Corporate')),
              Flag.Ever.Had.Quotes = factor(Flag.Ever.Had.Quotes),
              Flag.Had.Quote.In.Window = factor(Flag.Had.Quote.In.Window),
              Quantity.Log.x = log(Quantity),
              Nprod.Log.x = log(Nprod),
              Product.Type.Family.Services = factor(as.integer(Product.Type == 'Services')),
              PL9Ff = factor(as.integer(PL == '9F')),
              Product.Type.Family.Unit.Commercial.Desktops = factor(as.integer(Product.Type == 'Unit' & Product.Family == 'Commercial Desktops')),
              Customer.Segment.Enterprise = factor(as.integer(Customer.Segment == 'Enterprise')),
              Cannibalization.PL.NP.Log.1.plus.x = log1p(Cannibalization.PL.NP),
              d.Lagged.BD.Qty = factor(as.integer(Lagged.BD.Qty > 0)),
              Miscellaneous.Charge.Code.72G = factor(as.integer(Miscellaneous.Charge.Code == '72G')),
              Product.Type.Family.Commercial.Accessories = factor(as.integer(Product.Type == 'Other')),
              PLTAf = factor(as.integer(PL == 'TA')),
              Deal.Type.CSAT = factor(as.integer(str_detect(Deal.Description, fixed('Sat', ignore_case = T)))),
              Number.of.parts.Log.1.plus.x = log1p(Number.of.parts),
              Product.Type.Family.Display = factor(as.integer(Product.Type == 'Display'))
            )
        }
      } else {
        design_matrix <- df_grid %>%
          transmute(
            Discount = Discount,
            GMp = GMp,
            Active = Active,
            Flag.Ever.Had.Claims = Flag.Ever.Had.Claims,
            Flag.Ever.Had.Claims.SKU = Flag.Ever.Had.Claims.SKU,
            Flag.Ever.Had.Quotes = Flag.Ever.Had.Quotes,
            Flag.Ever.Had.Quotes.SKU = Flag.Ever.Had.Quotes.SKU,
            Deal.Gross.Revenue.Log1p = log1p(Deal.Gross.Revenue),
            Duration.Log1p = log1p(Duration),
            LP.Log1p = log1p(LP),
            Nprod.Log1p = log1p(Nprod),
            Quantity.Log1p = log1p(Quantity),
            Configuration.Type.BTO.Custom = as.numeric(Configuration.Type == 'BTO Custom'),
            Configuration.Type.BTO.Normal = as.numeric(Configuration.Type == 'BTO Normal'),
            Configuration.Type.BTO.Smart.Buy = as.numeric(Configuration.Type == 'BTO Smart Buy'),
            Configuration.Type.CTO.Bom.3 = as.numeric(Configuration.Type == 'CTO Bom 3'),
            Configuration.Type.CTO.Bom.5 = as.numeric(Configuration.Type == 'CTO Bom 5'),
            Configuration.Type.CTO.Line.Item = as.numeric(Configuration.Type == 'CTO Line Item'),
            Configuration.Type.CTO.Normal = as.numeric(Configuration.Type == 'CTO Normal'),
            CTO.or.BTO.BTO = as.numeric(CTO.or.BTO == 'BTO'),
            CTO.or.BTO.CTO = as.numeric(CTO.or.BTO == 'CTO'),
            Deal.Miscellaneous.Charge.Code.Deal.Type.Name.MIXED.Y = as.numeric(Deal.Miscellaneous.Charge.Code.Deal.Type.Name == 'MIXED-Y'),
            Deal.Miscellaneous.Charge.Code.Deal.Type.Name.REBATE = as.numeric(Deal.Miscellaneous.Charge.Code.Deal.Type.Name == 'REBATE'),
            Deal.Miscellaneous.Charge.Code.Deal.Type.Name.UPFRONT = as.numeric(Deal.Miscellaneous.Charge.Code.Deal.Type.Name == 'UPFRONT'),
            Product.Type.Display = as.numeric(Product.Type == 'Display'),
            Product.Type.Other = as.numeric(Product.Type == 'Other'),
            Product.Type.Services = as.numeric(Product.Type == 'Services'),
            Product.Type.Unit = as.numeric(Product.Type == 'Unit'),
            Unit.Type.Commercial.Desktops = as.numeric(Product.Family == 'Commercial Desktops'),
            Unit.Type.Commercial.Notebooks = as.numeric(Product.Family == 'Commercial Notebooks'),
            Unit.Type.Mobile.Workstations = as.numeric(Product.Family == 'Mobile Workstations'),
            Unit.Type.Other = as.numeric(Product.Family == 'Other'),
            Unit.Type.Workstations = as.numeric(Product.Family == 'Workstations')
          ) %>%
          as.matrix()
      }
    } else if (country == 'EU') {
      df_grid <- df_grid %>%
        replace_na(list(Processor.Flag = 'no_info')) %>%
        mutate(
          Configuration.Type = paste(CTO.or.BTO, '-', Smart.or.Bom),
          PL = if_else(PL %in% c('5U', '6U', '7F', '9F', '9H', 'AN', 'BO', 'DG', 'GA', 'I1', 'IK', 'IL', 'MG', 'MP', 'TA', 'TB'), PL, 'Other'),
          Product.Type = recode(Product.Type, `Remove from dashboard` = 'Other'),
          Product.Type.Family = if_else(Product.Type == 'Unit',
                                        recode(Product.Family,
                                               `Commercial Notebooks` = 'Unit.Commercial.Notebooks',
                                               `Commercial Desktops` = 'Unit.Commercial.Desktops',
                                               `Mobile Workstations` = 'Unit.Mobile.Workstations',
                                               `Workstations` = 'Unit.Workstations',
                                               .default = 'Unit.Other'),
                                        Product.Type),

          # Fix issue with 0s
          Delta.Discount = if_else(Discount - Most.Recent.Discount == 0, 0.0000001, Discount - Most.Recent.Discount),
          GMp = if_else(GMp == 0, 0.0000001, GMp),

          # Specify factor levels for one-hot encoding
          Business.Model.Description2 = factor(Business.Model.Description2, levels = c('Direct', 'Indirect', 'Single Tier Model', 'Volume Channel')),
          Configuration.Type = factor(Configuration.Type, levels = c('BTO - Custom', 'BTO - Normal', 'BTO - Top Value', 'CTO - Bom 3', 'CTO - Line Item')),
          Country.Region = factor(Country.Region, levels = c('CE', 'ISE', 'NE', 'SE', 'UK & I')),
          Customer.Segment = factor(Customer.Segment, levels = c('CEM', 'Global', 'PUBLIC SECTOR', 'Small Medium Business', 'Unidentified')),
          Deal.Miscellaneous.Charge.Code.Deal.Type.Name = factor(Deal.Miscellaneous.Charge.Code.Deal.Type.Name, levels = c('MIXED-Y', 'REBATE', 'UPFRONT')),
          Eclipse.Customer.Country.Code = factor(Eclipse.Customer.Country.Code, levels = c('CH', 'ES', 'FR', 'IT', 'NL', 'NO', 'DE', 'DK', 'GB', 'IE', 'LU', 'AT', 'BE', 'BG', 'BY', 'CY', 'CZ', 'EE', 'FI', 'GR', 'HR', 'HU', 'IL', 'LT', 'LV', 'MT', 'PL', 'PT', 'RO', 'RS', 'RU', 'SE', 'SI', 'SK', 'TR', 'UA')),
          Flag.Ever.Had.Claims = factor(Flag.Ever.Had.Claims, levels = c('0', '1')),
          Miscellaneous.Charge.Code = factor(Miscellaneous.Charge.Code, levels = c('07', '09R', '15R', '26R', '27R', '69T', '72', '72G', '72M', '72R', '74', '74B', '77', '77E', '77R', '78', 'A9', 'A9M', 'A9R', 'A9U', 'G9', '73', '78R', 'A9B', 'P72', '04', '10', '78E')),
          PL = factor(PL, levels = c('5U', '6U', '7F', '9F', '9H', 'AN', 'BO', 'DG', 'GA', 'I1', 'IK', 'IL', 'MG', 'MP', 'Other', 'TA', 'TB')),
          Processor.Flag = factor(Processor.Flag, levels = c('FALSE', 'no_info', 'TRUE')),
          Product.Type.Family = factor(Product.Type.Family, levels = c('Display', 'Other', 'Services', 'Unit.Commercial.Desktos', 'Unit.Commercial.Notebooks', 'Unit.Mobile.Workstations', 'Unit.Other', 'Unit.Workstations')),
        )

      if (model_type == 'Consumption') {
        features <- c(
          'Discount',
          'Delta.Discount',
          'AVG.Quoted.Discount.USD',
          'GMp',
          'List.Price',
          'Deal.Gross.Revenue',
          'Duration',
          'Nprod',
          'Display.Rate',
          'Quantity',
          'District.Manager.WR',
          'Business.Model.Description2',
          'Configuration.Type',
          'Customer.Segment',
          'PL',
          'Deal.Miscellaneous.Charge.Code.Deal.Type.Name',
          'Miscellaneous.Charge.Code',
          'Processor.Flag',
          'Product.Type.Family',
          'Eclipse.Customer.Country.Code',
          'Country.Region',
          'Flag.Ever.Had.Claims'
        )
      } else if (model_type == 'Attainment') {
        features <- c(
          'Discount',
          'Delta.Discount',
          'GMp',
          'Quantity',
          'COGS',
          'Lagged.BD.Qty',
          'Display.Rate',
          'Deal.Gross.Revenue',
          'List.Price',
          'Gross.Revenue',
          'Duration',
          'Nconfig',
          'Nprod',
          'Prop.COVID',
          'Business.Model.Description2',
          'Eclipse.Customer.Country.Code',
          'Customer.Segment',
          'Configuration.Type',
          'PL',
          'Flag.Ever.Had.Claims',
          'Miscellaneous.Charge.Code'
        )
      }

      df_grid <- df_grid %>% select(all_of(features))

      #design_matrix <- stats::predict(dummyVars('~ .', df_grid), df_grid)

      dummy_variables <- function(df_grid) {
        #' Create dummy variables
        #'
        #' @param df_grid A data frame.
        #' @return A data frame with all non-numeric variables as dummies.

        design_matrix <-
          df_grid %>%
          fastDummies::dummy_cols(ignore_na = TRUE, remove_selected_columns = TRUE) %>%
          mutate_all(as.numeric) %>%
          as.matrix() %>%
          setNames(colnames(.) %>% str_replace_all("_", "."))

        return(design_matrix)

      }

      design_matrix <- dummy_variables(df_grid)
      design_matrix <- as(as.matrix(design_matrix), "dgCMatrix")

    }

    return(design_matrix)
  }

  if (is.null(df_grid))
    return()

  if (country == 'US') {
    if (customer_segment != 'PUBLIC SECTOR') {
      consumption_model <- private_sector_consumption_model
      attainment_model <- private_sector_attainment_model
      consumption_design_matrix <- create_design_matrix(df_grid, country, customer_segment, 'Consumption')
      attainment_design_matrix <- create_design_matrix(df_grid, country, customer_segment, 'Attainment')
      df_grid <- df_grid %>%
        mutate(Consumption.Pred = stats::predict(consumption_model, consumption_design_matrix),
               Attainment.Pred = exp(stats::predict(attainment_model, attainment_design_matrix)),
               BD.Qty.Pred = pmax(Consumption.Pred * Attainment.Pred, 0))
    } else {
      consumption_model <- public_sector_consumption_model
      attainment_model <- public_sector_attainment_model
      design_matrix <- create_design_matrix(df_grid, country, customer_segment)
      df_grid <- df_grid %>%
        mutate(Consumption.Pred = stats::predict(consumption_model, design_matrix),
               Attainment.Pred = stats::predict(attainment_model, design_matrix),
               BD.Qty.Pred = pmax(Consumption.Pred * Attainment.Pred * Quantity, 0))
    }
  } else if (country == 'EU') {
    consumption_design_matrix <- create_design_matrix(df_grid, country, customer_segment, 'Consumption')
    attainment_design_matrix <- create_design_matrix(df_grid, country, customer_segment, 'Attainment')
    df_grid <- df_grid %>%
      mutate(Consumption.Pred = stats::predict(consumption_model, consumption_design_matrix),
             Consumption.Pred = if_else(Consumption.Pred > 0.3, 1, 0),
             Attainment.Pred = exp(stats::predict(attainment_model, attainment_design_matrix)),
             BD.Qty.Pred = pmax(Consumption.Pred * Attainment.Pred, 0))
  }

  df_grid <- df_grid %>%
    mutate(GM.Pred = (NP - Contra - COGS) * BD.Qty.Pred / Quantity)

  return (df_grid)
}

optimize_gm <- function(df_grid) {
  #' Run the optimization procedure
  #'
  #' @param df_grid A data frame.
  #' @param country A country.
  #' @param customer_segment A customer segment.
  #' @return A data frame with the optimal discount for each configuration.

  if (is.null(df_grid))
    return()

  df_optimized <- df_grid %>%
    rename(Async.Typical.GMp = GMp) %>%
    group_by(Product.Configuration.Id) %>%
    filter(GM.Pred == max(GM.Pred)) %>%
    arrange(NP) %>%
    distinct(Product.Configuration.Id, .keep_all = T) %>%
    ungroup()

  return(df_optimized)
}

create_guidance <- function(input, df_optimized, df_out_of_scope, country) {
  #' Create a list of guidance
  #'
  #' @param df_optimized A dataframe.
  #' @param country A country.
  #' @return A list.

  if (!is.null(df_optimized)) {
    df_optimized <- df_optimized %>%
      transmute(Product.Configuration.Id = Product.Configuration.Id,
                Floor.Discount = 1 + COGS / ((Sync.Floor.GMp - 1) * LP),
                Typical.Discount = 1 + COGS / ((Sync.Typical.GMp - 1) * LP),
                Expert.Discount = 1 + COGS / ((Sync.Expert.GMp - 1) * LP))
  } else {
    df_optimized <- df_out_of_scope %>%
      transmute(Product.Configuration.Id = Product.Configuration.Id,
                Floor.Discount = NA_real_,
                Typical.Discount = NA_real_,
                Expert.Discount = NA_real_)
  }

  guidance <- input %>%
    group_by(Product.Configuration.Id) %>%
    mutate(Discount.Standard.Override = !all(COGS > 0) | (Country == 'United States' & PL %in% c('8W', 'UT')) | !is.na(Banded.Product.Flag) & Banded.Product.Flag == 'Y') %>%
    ungroup() %>%
    left_join(df_optimized, by = 'Product.Configuration.Id') %>%
    mutate(Discount.Standard = Discount.Standard.USD / LP,
           Floor.Discount = if_else(Discount.Standard.Override, Discount.Standard, Floor.Discount),
           Typical.Discount = if_else(Discount.Standard.Override, Discount.Standard, Typical.Discount),
           Expert.Discount = if_else(Discount.Standard.Override, Discount.Standard, Expert.Discount)) %>%
    transmute(
      lineId = lineId,
      SKU = SKU,
      FINAL_PRICE_EXPERT = (1 - Expert.Discount) * LP,
      FINAL_PRICE_TYPICAL = (1 - Typical.Discount) * LP,
      FINAL_PRICE_FLOOR = (1 - Typical.Discount) * LP,
      FINAL_DISCOUNT_EXPERT = Expert.Discount,
      FINAL_DISCOUNT_TYPICAL = Typical.Discount,
      FINAL_DISCOUNT_FLOOR = Typical.Discount,
      FINAL_MARGIN_EXPERT = if_else(Discount.Standard.Override, 1, (FINAL_PRICE_EXPERT - COGS) / FINAL_PRICE_EXPERT),
      FINAL_MARGIN_TYPICAL = if_else(Discount.Standard.Override, 1, (FINAL_PRICE_TYPICAL - COGS) / FINAL_PRICE_TYPICAL),
      FINAL_MARGIN_FLOOR = if_else(Discount.Standard.Override, 1, (FINAL_PRICE_FLOOR - COGS) / FINAL_PRICE_FLOOR),
      GUID_METHOD_TYPE = 'uspssync',
      price = FINAL_PRICE_EXPERT
    ) %>%
    mutate(across(where(is.numeric) & !lineId, ~trimws(format(round(., 8), nsmall = 8))))

  # Convert data frame to list for final output
  list(columns = names(guidance), data = asplit(trimws(as.matrix(unname(guidance))), 1))
}

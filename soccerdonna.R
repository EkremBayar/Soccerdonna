library(tidyverse)
library(rvest)
library(openxlsx)

# Competition List
soccerdonna_competition <- function(){
  url <- "https://www.soccerdonna.de/de/2010/startseite/wettbewerbeDE.html"
  page <- rvest::read_html(url)
  tbl <- page %>% rvest::html_elements("table.tabelle_grafik")
  get_competition <- function(html, competition_type){
    
    tr <- html %>% rvest::html_elements("tr")
    
    res <- purrr::map_df(tr, function(i){
      
      competition_type <- competition_type
      
      if(i %>% rvest::html_attr("class") == "dunkel"){
        country_name <- i %>% rvest::html_elements("td.al.fb img") %>% rvest::html_attr("title")
        country_img <- i %>%rvest:: html_elements("td.al.fb img") %>% rvest::html_attr("src")
        league_img <- NA_character_
        league_name <- NA_character_
        league_url <- NA_character_
        league_tier <- NA_character_
      }else{
        country_name <- NA_character_
        country_img <- NA_character_
        league_img <- i %>% rvest::html_element("td") %>% rvest::html_attr("style") %>% stringr::str_split_i("url\\('", -1) %>% stringr::str_split_i("'\\)", 1)
        league_name <- i %>% rvest::html_elements("td.al a") %>% rvest::html_attr("title")
        league_url <- i %>% rvest::html_elements("td.al a") %>% rvest::html_attr("href") %>% paste0("https://www.soccerdonna.de", .)
        league_tier <- i %>% rvest::html_element("td.ac") %>% rvest::html_text()
      }
      
      tibble::tibble(
        competition_type,
        country_name, 
        country_img,
        league_img,
        league_name,
        league_url,
        league_tier
      )
      
    })
    
    return(res)
  }
  
  comp_df <- dplyr::bind_rows(
    get_competition(tbl[[1]], "National"),
    get_competition(tbl[[2]], "International")
  ) %>% 
    tidyr::fill(country_name) %>% 
    tidyr::fill(country_img) %>% 
    dplyr::filter(!is.na(league_name)) %>% 
    dplyr::mutate(
      country_name = ifelse(competition_type == "International","International",country_name),
      country_img = ifelse(competition_type == "International",NA_character_,country_img),
      country_id = stringr::str_split_i(stringr::str_split_i(country_img, "\\.gif", 1), "/", -1),
      country_id = ifelse(competition_type == "International",NA_character_,country_id),
      league_id = stringr::str_remove_all(stringr::str_split_i(stringr::str_split_i(league_url, "\\.html", 1), "/", -1), "wettbewerb_")
    ) %>% 
    dplyr::select(
      c("competition_type", "country_id", "country_name", "country_img", "league_id", "league_name", "league_tier", "league_url", "league_img")
    )
  
  return(comp_df)
}
competition_list <- soccerdonna_competition()

openxlsx::write.xlsx(competition_list, "soccerdonna_competition.xlsx")
competition_list <- read.xlsx("soccerdonna_competition.xlsx")



# Team List
soccerdonna_team <- function(competition_list){
  
  team_list <- purrr::map_df(1:nrow(competition_list), function(x){
    
    url <- competition_list$league_url[x]
    
    tryCatch({
      page <- rvest::read_html(url)
      
      # League
      tbl <- page %>% 
        rvest::html_element("table.standard_tabelle") %>% 
        rvest::html_elements("tbody tr")
      
      if(length(tbl) > 0){
        team_res <- purrr::map_df(1:length(tbl), function(i){
          td <- tbl[[i]] %>% rvest::html_elements("td")
          team_name <- td[[1]] %>% rvest::html_element("a") %>% rvest::html_attr("title")
          team_url <- td[[1]] %>% rvest::html_element("a") %>% rvest::html_attr("href") %>% paste0("https://www.soccerdonna.de", .)
          team_img <- td[[1]] %>% rvest::html_element("a img") %>% rvest::html_attr("src")
          team_id <- stringr::str_split_i(stringr::str_split_i(team_url, "\\.html", 1), "verein_", -1)
          national_team <- ifelse(competition_list$competition_type[x] == "National", "NO", "YES")#ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), "YES", "NO")
          team_id <- ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), stringr::str_split_i(team_id, "nationalmannschaft_", -1), team_id)
          tibble::tibble(
            team_id,
            team_name,
            team_url,
            team_img,
            national_team
          )
        })
        
        team_res <- dplyr::bind_cols(competition_list[x,], team_res)
      }else{
        
        # Cup Finale
        tbl <- page %>% rvest::html_elements(".tabelle_grafik")
        if(length(tbl) > 0){
          tbl <- tbl %>% .[[1]] %>% rvest::html_elements(".hell.lh .ac.s10 a") 
          team_name <- tbl %>% rvest::html_attr("title") %>% stringr::str_split_i("Begegnungen:", -1) %>% stringr::str_squish()
          team_url <- tbl %>% rvest::html_attr("href") %>% stringr::str_replace_all("spielplan/verein", "startseite/verein") %>% paste0("https://www.soccerdonna.de",.)
          team_id <- stringr::str_split_i(stringr::str_split_i(team_url, "\\.html", 1), "verein_", -1)
          national_team <- national_team <- ifelse(competition_list$competition_type[x] == "National", "NO", "YES")#ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), "YES", "NO")
          team_id <- ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), stringr::str_split_i(team_id, "nationalmannschaft_", -1), team_id)
          team_img <- paste0("https://www.soccerdonna.de/static/bilder_sd/wappen/",team_id,".png")
          temp1 <- dplyr::bind_cols(competition_list[x,], 
                                       tibble::tibble(
                                         team_id,
                                         team_name,
                                         team_url,
                                         team_img,
                                         national_team
                                       ))
        }else{
          temp1 <- NULL
        }
           
        tbl <- page %>% rvest::html_elements("table#vereine.tabelle_grafik tbody tr")
        if(length(tbl) > 0){
          temp2 <- purrr::map_df(1:length(tbl), function(i){
            td <- tbl[[i]] %>% rvest::html_elements("td")
            team_name <- td[[1]] %>% rvest::html_element("a") %>% rvest::html_attr("title")
            team_url <- td[[1]] %>% rvest::html_element("a") %>% rvest::html_attr("href") %>% paste0("https://www.soccerdonna.de", .)
            team_img <- td[[1]] %>% rvest::html_element("a img") %>% rvest::html_attr("src") %>% paste0("https://www.soccerdonna.de", .)
            team_id <- stringr::str_split_i(stringr::str_split_i(team_url, "\\.html", 1), "verein_", -1)
            national_team <- national_team <- ifelse(competition_list$competition_type[x] == "National", "NO", "YES")#ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), "YES", "NO")
            team_id <- ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), stringr::str_split_i(team_id, "nationalmannschaft_", -1), team_id)
            tibble::tibble(
              team_id,
              team_name,
              team_url,
              team_img,
              national_team
            )
          })
          temp2 <- dplyr::bind_cols(competition_list[x,], temp2)
        }else{
          #tbl <- page %>% rvest::html_elements(".standard_tabelle tr")
          #if(length(tbl) > 0){
            team_name <- c(
              page %>% rvest::html_elements(".standard_tabelle .ar a") %>% rvest::html_text(),
              page %>% rvest::html_elements(".standard_tabelle .wsnw~ td .s10") %>% rvest::html_text()
            )
            # if(sum(is.na(team_name))>0){
            #   team_name <- c(
            #     page %>% rvest::html_elements(".standard_tabelle .ar a") %>% rvest::html_attr("title"),
            #     page %>% rvest::html_elements(".standard_tabelle .wsnw~ td .s10") %>% rvest::html_attr("title")
            #   )
            # }
            
            team_url <- c(
              page %>% rvest::html_elements(".standard_tabelle .ar .s10") %>% rvest::html_attr("href"),
              page %>% rvest::html_elements(".standard_tabelle .wsnw~ td .s10")  %>% rvest::html_attr("href")
            ) %>% paste0("https://www.soccerdonna.de", .) %>% stringr::str_replace_all("spielplan/verein", "startseite/verein") 
            team_id <- stringr::str_split_i(stringr::str_split_i(team_url, "\\.html", 1), "verein_", -1)
            team_img <- c(
              page %>% rvest::html_elements(".ar+ .wid10 img") %>% rvest::html_attr("src"),
              page %>% rvest::html_elements(".wsnw+ .wid10 img") %>% rvest::html_attr("src")
            )
            national_team <- national_team <- ifelse(competition_list$competition_type[x] == "National", "NO", "YES")#ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), "YES", "NO")
            team_id <- ifelse(stringr::str_detect(team_id, "nationalmannschaft_"), stringr::str_split_i(team_id, "nationalmannschaft_", -1), team_id)
            
            temp2 <- dplyr::bind_cols(competition_list[x,], 
                                         tibble::tibble(
                                           team_id,
                                           team_name,
                                           team_url,
                                           team_img,
                                           national_team
                                         ))
            
          #}
        }
        
        team_res <- dplyr::bind_rows(temp1, temp2)
        

        
      }

    },error=function(e){
      message(paste0(x,": ",e, "\n", url))
      team_res <- NULL
    })
    
  })
  
  if(is.data.frame(team_list)){
    team_list <- team_list %>% 
      group_by(team_id) %>% 
      fill(team_img, .direction = "updown") %>% 
      fill(team_img, .direction = "downup") %>% 
      do(head(., 1)) %>% 
      ungroup() %>% 
      arrange(country_name, league_tier, league_name, team_name)
  }
  
  return(team_list)
  
}

team_list <- soccerdonna_team(competition_list)
openxlsx::write.xlsx(team_list, "soccerdonna_team.xlsx")
team_list <- openxlsx::read.xlsx("soccerdonna_team.xlsx")

# Player List
soccerdonna_player <- function(team_list){
  player_df <- purrr::map_df(1:nrow(team_list), function(x){
    tryCatch({
      url <- team_list$team_url[x]
      page <- rvest::read_html(url)
      tr <- page %>% rvest::html_elements("table#spieler tbody tr")
      if(length(tr) > 0){
        player_df <- purrr::map_df(tr, function(i){
          player_name <- i %>% rvest::html_element("td.al tr td a") %>% rvest::html_attr("title")
          player_url <- i %>% rvest::html_element("td.al tr td a") %>% rvest::html_attr("href") %>% paste0("https://www.soccerdonna.de", .)
          player_img <- i %>% rvest::html_element("td.al tr td img") %>% rvest::html_attr("src")
          player_id <- stringr::str_split_i(stringr::str_split_i(player_url, "\\.html", 1), "spieler_", -1)
          tibble::tibble(
            player_id,
            player_name,
            player_url,
            player_img
          )
        }) %>% 
          dplyr::filter(player_id != "https://www.soccerdonna.deNA")
        player_df <- dplyr::bind_cols(team_list[x, ],player_df)
      }else{
        player_df <- NULL
      }
      
    },error=function(e){
      message(paste0(x,":",e, "\n", team_list$team_url[x]))
      player_df <- NULL
    })
    
  })
  
  if(is.data.frame(player_df)){
    player_df <- player_df %>% 
      group_by(player_id) %>% 
      do(head(., 1)) %>% 
      ungroup()
  }
  
  return(player_df)
}

player_list <- soccerdonna_player(team_list)

openxlsx::write.xlsx(player_list, "soccerdonna_player.xlsx")


player_list <- openxlsx::read.xlsx("soccerdonna_player.xlsx")

# Player Bio
url <- "https://www.soccerdonna.de/de/juliana-capao/profil/spieler_55606.html"
url <- "https://www.soccerdonna.de/de/catarina-amado/profil/spieler_42246.html"
url <- "https://www.soccerdonna.de/en/zecira-musovic/profil/spieler_18814.html"
url <- "https://www.soccerdonna.de/en/katharina-hruby/profil/spieler_100025.html"
url <- "https://www.soccerdonna.de/en/barbara/profil/spieler_100.html"
soccerdonna_player_bio <- function(url){
  
  if(stringr::str_sub(url, 1, 29) != "https://www.soccerdonna.de/en"){
    url <- paste0("https://www.soccerdonna.de/en", stringr::str_sub(url, 30, stringr::str_count(url)))
  }
  
  
  
  page <- rvest::read_html(url)
  
  # From German to English
  # translate_german_to_english <- c(
  #   "alter" = "age", 
  #   "debut_club" = "debut_club", 
  #   "fuss" = "foot", 
  #   "geburtsdatum" = "date_of_birth", 
  #   "geburtsname" = "name_in_native_country", 
  #   "geburtsort" = "place_of_birth", 
  #   "grosse" = "height", 
  #   "homepage" = "homepage", 
  #   "letzte_verlangerung", 
  #   "marktwert" = "market_value", 
  #   "nationalitat" = "citizenship",
  #   "position" = "position", 
  #   "vertrag_bis" = "contract_until", 
  #   "vertragsoption" = "contract_option",
  #   "x2_verein_rn" = "second_club_name",
  #   "letzte_verlangerung" = "last_contract_extension",
  #   "letztes_spiel" = "last_match"
  # )
  
  # corrected_variable <- c(
  #   "x" = "last_contract_extension",
  #   "x2_club_number" = "second_club_name"
  # )
  
  
  # Player Id
  soccerdonna_id <- stringr::str_remove_all(basename(url), "spieler_|\\.html")
  # Player Image
  player_image <- tryCatch({
    page %>% rvest::html_elements(".tabelle_grafik .minifoto") %>% rvest::html_attr("src")
  },error=function(e){
    "https://www.soccerdonna.de/static/bilder_sd/spielerfotos/somebody.jpg"
  })
  # Player Name Jersey
  player_name_jersey <- page %>% 
    rvest::html_elements("div#centerbig .tabelle_spieler") %>% .[[1]] %>% 
    rvest::html_elements("tr") %>% .[[1]] %>% rvest::html_elements("h1") %>% rvest::html_text() %>% stringr::str_squish()
  # Jersey
  player_jersey <- suppressWarnings(invisible(readr::parse_number(player_name_jersey)))
  # Name
  player_name <- gsub("[[:digit:]]+ ", "", player_name_jersey) %>% stringr::str_remove_all("\\?") %>% stringr::str_squish()
  
  # ActivePlayer? Retired?
  active_player <- page %>% rvest::html_elements("div#centerbig .tabelle_spieler") %>% .[[1]] %>% rvest::html_elements("tr")
  last_club <- active_player[stringr::str_detect(as.character(active_player), "Last club")]
  if(length(last_club) > 0){
    last_club_id <- last_club %>% rvest::html_element("a") %>% rvest::html_attr("href") %>% stringr::str_split_i("/verein_", -1) %>% stringr::str_remove("\\.html")
    last_club <- last_club %>% rvest::html_element("a") %>% rvest::html_text()
  }else{
    last_club_id <- NA_character_
    last_club <- NA_character_
  }
  active_player <- active_player[stringr::str_detect(as.character(active_player), "End of career")]
  active_player <- ifelse(length(active_player) > 0, "NO", "YES")
  
  # Gathering first info
  player_df <- data.frame(
    X1 = c("soccerdonna_id", "jersey", "player_name", "image_url", "url", "active_player", "last_club_id", "last_club"),
    X2 = c(soccerdonna_id, player_jersey, player_name, player_image, url, active_player, last_club_id, last_club)
  )
  # Player Bio
  player_bio <- page %>% 
    rvest::html_elements(".tabelle_grafik td.al.vt .tabelle_spieler") %>% 
    rvest::html_table() %>% 
    .[[1]] %>% 
    dplyr::mutate(
      # Clean name
      X1 = janitor::make_clean_names(X1),
      X2 = stringr::str_squish(X2),
      X1 = dplyr::case_when(
        .data$X1 == "x" ~ "last_contract_extension",
        .data$X1 == "x2_club_number" ~ "second_club_name",
        .default = .data$X1
      )
    )
  
  # Player Citizenship
  if(length(which(player_bio$X1 == "nationality")) > 0){
    # Citizenship
    citizenship <- page %>% rvest::html_elements(".tabelle_grafik td.al.vt .tabelle_spieler tr")
    citizenship <- citizenship[which(stringr::str_detect(as.character(citizenship), "Nationality"))]
    if(length(citizenship) > 0){
      citizenship_ids <- citizenship %>% rvest::html_elements("img") %>% rvest::html_attr("src") %>% stringr::str_split_i("/", -1) %>% stringr::str_remove_all("\\.gif")
      citizenship <- citizenship %>% rvest::html_elements("img") %>% rvest::html_attr("title")
      if(length(citizenship) == 1){
        citizenship_ids <- c(citizenship_ids, NA_character_)
        citizenship <- c(citizenship, NA_character_)
      }
    }else{
      citizenship_ids <- rep(NA_character_, 2)
      citizenship <- rep(NA_character_, 2)
    }
  }else{
    citizenship_ids <- rep(NA_character_, 2)
    citizenship <- rep(NA_character_, 2)
  }
  player_citizenship <- data.frame(
    X1 = c("citizenship_id1", "citizenship_id2", "citizenship1", "citizenship2"),
    X2 = c(citizenship_ids, citizenship)
  )
  
  # Second Club
  if(length(which(player_bio$X1 == "second_club_name")) > 0){
    second_club_id <- page %>% rvest::html_elements(".tabelle_grafik td.al.vt .tabelle_spieler tr")
    second_club_id <- second_club_id[which(stringr::str_detect(as.character(second_club_id), "2.Club"))]
    if(length(second_club_id) > 0){
      second_club_id <- second_club_id %>% rvest::html_element("a") %>% rvest::html_attr("href") %>% stringr::str_split_i("/verein_", -1) %>% stringr::str_remove("\\.html")
    }else{
      second_club_id <- NA_character_
    }
  }else{
    second_club_id <- NA_character_
  }
  second_club_id <- data.frame(X1 = "second_club_id", X2 = second_club_id)
  
  # Club & League info
  club_league <- page %>% 
    rvest::html_elements("div#centerbig .tabelle_spieler") %>% .[[1]] %>% 
    rvest::html_elements("tr") %>% .[[2]] %>% 
    rvest::html_elements("a") 
  if(length(club_league) > 0){
    club_league_id <- club_league %>% rvest::html_attr("href") %>% stringr::str_split_i("/verein_|/wettbewerb_",-1) %>% stringr::str_remove_all("\\.html")
    club_league_name <- club_league %>% rvest::html_text()
    if(length(club_league_id) == 1){
      club_league_id <- c(club_league_id, NA_character_)
      club_league_name <- c(club_league_name, NA_character_)
    }
  }else{
    club_league_id <- rep(NA_character_, 2)
    club_league_name <- rep(NA_character_, 2)
  }
  club_league <- data.frame(
    X1 = c("current_club_id", "league_id", "current_club", "league"),
    X2 = c(club_league_id, club_league_name)
  )
  
  # National Team
  national_team <- page %>% rvest::html_elements("div#centerbig .tabelle_spieler") %>% .[[1]] %>% rvest::html_elements("tr")
  check <- which(stringr::str_detect(as.character(national_team), "Current national player"))
  if(length(check) > 0){
    national_team_name <- national_team[check] %>% rvest::html_text() %>% stringr::str_remove_all("Current national player\\:") %>% stringr::str_squish()
    national_team_id <- national_team[check] %>% rvest::html_element("a") %>% rvest::html_attr("href") %>% stringr::str_split_i("/nationalmannschaft_", -1) %>% stringr::str_remove_all("\\.html") 
    if(national_team_name == "Nein"){national_team_name <- NA_character_}
  }else{
    national_team_name <- NA_character_
    national_team_id <- NA_character_
  }
  national_team <- data.frame(
    X1 = c("national_team_id", "national_team"),
    X2 = c(national_team_id, national_team_name)
  )
  
  
  # Bind Player Details
  check_cols <- c(
    "soccerdonna_id", "jersey", "player_name",  "name_in_native_country", 
    "date_of_birth", "age", "height", "foot", "position","position_class","main_position", "market_value",
    "place_of_birth", "citizenship_id1", "citizenship_id2", "citizenship1", "citizenship2",
    "league_id", "league", "current_club_id", "current_club", "debut_club", "contract_until", 
    "contract_option", "last_contract_extension", 
    "second_club_id", "second_club_name", "national_team_id", "national_team", 
    "homepage", "image_url", "url", 
    "active_player", "last_match", "last_club_id", "last_club"
  )
  
  # Bind Dataframe
  df <- dplyr::bind_rows(
    player_df,
    player_bio,
    player_citizenship,
    second_club_id,
    club_league,
    national_team
  ) 
  df <- df%>% 
    # Add missing information
    dplyr::bind_rows(
      data.frame(X1 = check_cols, X2 = rep(NA_character_, length(check_cols))) %>% dplyr::filter(!.data$X1 %in% df$X1)
    ) %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2) %>% 
    tidyr::separate("position", c("position_class", "main_position"), sep = "-") %>% 
    dplyr::mutate_at(c("position_class", "main_position"), stringr::str_squish) %>% 
    dplyr::mutate(
      height = stringr::str_remove_all(.data$height, "\\,"),
      market_value = as.integer(stringr::str_squish(stringr::str_remove_all(stringr::str_replace_all(.data$market_value, "unknown", NA_character_), "€|\\."))),
    ) %>% 
    # Logical
    dplyr::mutate_if(is.logical, as.character) %>% 
    # List
    dplyr::mutate_if(is.list, as.character) %>% 
    # Date Format
    dplyr::mutate_at(c("date_of_birth", "contract_until", "debut_club", "last_contract_extension", "last_match"), lubridate::dmy) %>% 
    # Integer Format
    dplyr::mutate_at(c("soccerdonna_id", "jersey","age", "height", "citizenship_id1", "citizenship_id2", "second_club_id", "current_club_id", "national_team_id", "last_club_id"), as.integer) %>% 
    # Sort Columns
    dplyr::select(dplyr::any_of(check_cols)) %>% 
    suppressWarnings()
  
  return(df)
}


dput(names(df))

df%>% 
  tidyr::separate("position", c("position_class", "main_position"), sep = "-") %>% 
  dplyr::mutate_at(c("position_class", "main_position"), stringr::str_squish) %>% 
  dplyr::mutate(
      ) %>% 
  suppressWarnings() %>% View


b <- a %>% 
  tidyr::separate("position", c("position_class", "main_position"), sep = "-") %>% 
  dplyr::mutate_at(c("position_class", "main_position"), stringr::str_squish) %>% 
  mutate(
    # Position Class
    position_class = dplyr::case_when(
      position_class == "Tor" ~ "Goalkeeper",
      position_class == "Abwehr" ~ "Defender",
      position_class == "Mittelfeld" ~ "Midfield",
      position_class == "Angriff" ~ "Attack",
      .default = .data$position_class
    ),
    # Main Position
    main_position = dplyr::case_when(
      tolower(main_position) == "defensives mittelfeld" ~ "Defensive Midfield",
      tolower(main_position) == "offensives mittelfeld" ~ "Attacking Midfield",
      tolower(main_position) == "zentrales mittelfeld" ~ "Central Midfield",
      tolower(main_position) == "Angriff" ~ "Attack",
      tolower(main_position) == "Angriff" ~ "Attack",
      tolower(main_position) == "Angriff" ~ "Attack",
      tolower(main_position) == "Angriff" ~ "Attack",
      tolower(main_position) == "Angriff" ~ "Attack",
      .default = .data$main_position
    ),
    # Market Value
    market_value = as.integer(stringr::str_squish(stringr::str_remove_all(stringr::str_replace_all(.data$market_value, "unknown", NA_character_), "€|\\."))),
    # Foot
    foot = dplyr::case_when(
      foot == "links" ~ "left",
      foot == "rechts" ~ "right",
      foot == "beidfüßig" ~ "both",
      .default = .data$foot
    ),
    # Contract Options
    contract_option = dplyr::case_when(
      stringr::str_detect(tolower(contract_option), "beidseitig") ~ paste0("mutual ", suppressWarnings(invisible(readr::parse_number(contract_option))), "-year extension"),
      stringr::str_detect(tolower(contract_option), "ein weiteres") ~ "one additional year",
      stringr::str_detect(tolower(contract_option), "kaufoption") ~ "purchase option",
      stringr::str_detect(tolower(contract_option), "spielerseitig") ~ paste0("player option for", suppressWarnings(invisible(readr::parse_number(contract_option))), " year"),
      stringr::str_detect(tolower(contract_option), "vereinsseitig") ~ paste0("club option for ", suppressWarnings(invisible(readr::parse_number(contract_option))), "year", ifelse(suppressWarnings(invisible(readr::parse_number(.data$contract_option))) > 1, "s", "")),
      .default = .data$contract_option
    )
  ) %>% 
  suppressWarnings()


#,
# # Position
# position = dplyr::case_when(
#   # Position Class
#   stringr::str_detect(.data$position, "Tor") ~ stringr::str_replace_all(.data$position, "Tor", "Goalkeeper"),
#   stringr::str_detect(.data$position, "Abwehr") ~ stringr::str_replace_all(.data$position, "Abwehr", "Defender"),
#   stringr::str_detect(.data$position, "Mittelfeld") ~ stringr::str_replace_all(.data$position, "Mittelfeld", "Midfield"),
#   stringr::str_detect(.data$position, "Angriff") ~ stringr::str_replace_all(.data$position, "Angriff", "Attack"),
#   .default = .data$position
# )

Abwehr - Innenverteidigung: Centre-Back


b %>% select(url, player_name, position_class, main_position) %>% 
  group_by(position_class, main_position) %>% 
  do(head(., 1)) %>%
  arrange(position_class, main_position) %>% view



b$position %>% unique %>% sort

stringr::str_replace_all(b$position %>% unique %>% sort, "Mittelfeld", "Midfield")


translate_position_german_to_english <- c(
  "Tor" = "Goalkeeper",
  "Abwehr" = "Defender",
  "Mittelfeld" = "Midfield",
  "Angriff" = "Attack",
  "TW" = "Goalkeeper",
  "RV" = "Right-Back",
  "LV" = "Left-Back",
  "ZM" = "Central Midfield",
  "DM" = "Defensive Midfield",
  "OM" = ,
  "Rechtsaußen" = "Right Winger",
  "Linksaußen" = "Left Winger",
  "MS" = "Centre-Forward",
  
)

offensives Mittelfeld

"Tor (TW)" = "Goalkeeper"
"Abwehr (RV)" = "Defender" RV Right-Back
LV Left-Back
Abwehr (IV) - "Centre-Back"
Mittelfeld (ZM), Central Midfield
Mittelfeld (DM)  Defensive Midfield
Angriff - Rechtsaußen, Attack, Right Winger
Angriff - Linksaußen, Attack, Left Winger
Angriff (MS) Centre-Forward


url <- "https://www.soccerdonna.de/de/christie-harrison-murray/profil/spieler_10426.html"
url <- "https://www.soccerdonna.de/de/katharina-hruby/profil/spieler_100025.html"
soccerdonna_player_bio(url) %>% view
url <- "https://www.soccerdonna.de/de/ece-tuerkoglu/profil/spieler_30283.html"
soccerdonna_player_bio(url)



library(doParallel)
library(foreach)
library(parallel)

numCores <- detectCores()
cl <- makeCluster(numCores, outfile = "")
registerDoParallel(cl)
pdata <- foreach(i = 1:nrow(player_list),
                 .packages = c("tidyverse")) %dopar% {
  print(i)
  tryCatch({
    
    url <- player_list$player_url[i]
    soccerdonna_player_bio(url)
  },error=function(e){
    message(e)
    NULL
  })
                   
}

# Cluster'ı kapatma
stopCluster(cl)
rm(cl, numCores)

a <- bind_rows(pdata)

write.csv(a, "soccerdonna_player_bio.csv", row.names = FALSE)

write.xlsx(a, "player_bio.xlsx")


df <- read.xlsx("player_bio.xlsx")

# Position
position_class = stringr::str_squish(stringr::str_split_i(.data$position, "-", 1)),
position_class = ifelse(.data$position_class == "", NA_character_, .data$position_class),






d <- lapply(1:length(pdata), function(i){
  pdata[[i]] %>% mutate_if(is.list, as.character)
  
}) %>% bind_rows



b <- pdata[[2756]] 

player_list[2756,]

pdata[[2756]] %>% View

player_df <- purrr::map_df(1:nrow(player_list), function(i){
  print(i)
  tryCatch({
    url <- player_list$player_url[i]
    soccerdonna_player_bio(url)
  },error=function(e){
    message(e)
    NULL
  })
  
})


# Soccerdonna içindeki tüm takımlar ve oyuncu listeleri için
all_team_ids <- paste0("https://www.soccerdonna.de/de/xx-xx/startseite/verein_",1:16084, ".html")
team_list %>% filter(team_id %in% 1:16084) %>% nrow














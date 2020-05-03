if (jjj < 10){topic_crossref <- paste0("#topic0",jjj)}
if (jjj > 9){topic_crossref <- paste0("#topic",jjj)}


cat("## Topic ", jjj, " - ", the_categories$subject[[jjj]], " {", topic_crossref, "}\n", sep="")

if(!the_categories$cat_num[jjj] == 13){
  cat_nam <- the_categories$cat_name[jjj]
}
if(the_categories$cat_num[jjj] == 13){
  temp_category_tibble <- tibble(
    topic = (jjj*100+1):(jjj*100+2))
  temp_category_tibble <- temp_category_tibble %>%
    inner_join(the_categories, by = "topic")
  cat_nam <- paste(temp_category_tibble$cat_name, sep = "/", collapse = "/")
}

cat("**Category**: ", cat_nam, "\n\n")

distinctive <- distinctive_topics[(jjj-1)*15 + 1, 1]
for (jj in 2:15){
  distinctive <- paste(distinctive, distinctive_topics[(jjj-1)*15 + jj, 1], sep = ", ")
}

cat("**Keywords**: ", distinctive, "\n\n")

require(toOrdinal)

cat("**Number of Articles**: ", overall_stats$r_count[jjj], " \n <br>", sep="")
cat("**Percentage of Total**: ", 100*overall_stats$r_percent[jjj], "%\n <br> ", sep="")
cat("**Rank**: ", toOrdinal(overall_stats$r_rank[jjj]), "\n\n", sep="")

cat("**Weighted Number of Articles**: ", overall_stats$w_count[jjj], "\n <br>", sep="")
cat("**Percentage of Total**: ", 100*overall_stats$w_percent[jjj], "%\n <br>", sep="")
cat("**Rank**: ", toOrdinal(overall_stats$w_rank[jjj]), "\n\n", sep="")

cat("**Mean Publication Year**: ", overall_stats$mean_y[jjj], "\n <br>", sep="")
cat("**Weighted Mean Publication Year**: ", overall_stats$wy[jjj], "\n <br>", sep="")
cat("**Median Publication Year**: ", overall_stats$median_y[jjj], "\n <br>", sep="")
cat("**Modal Publication Year**: ", overall_stats$modal_y[jjj], "\n\n", sep="")

cat("**Topic with Most Overlap**: ",
    the_categories$subject[closest_neighbour$othertopic[jjj]], 
    " (", 
    round(closest_neighbour$g[jjj],4),
    ")\n <br>",
    sep="")
cat("**Topic this Overlaps Most With**: ",
    the_categories$subject[closest_neighbour_inverse$topic[jjj]], 
    " (", 
    round(closest_neighbour_inverse$g[jjj],4),
    ")\n <br>",
    sep="")
cat("**Topic with Least Overlap**: ",
    the_categories$subject[furthest_neighbour$othertopic[jjj]], 
    " (", 
    round(furthest_neighbour$g[jjj],5),
    ")\n <br>",
    sep="")
cat("**Topic this Overlaps Least With**: ",
    the_categories$subject[furthest_neighbour_inverse$topic[jjj]], 
    " (", 
    round(furthest_neighbour_inverse$g[jjj],5),
    ")\n\n <br>",
    sep="")

##### This page is trying to find out how to set up Anova with 8-variable interaction
#### not as factor
lm.data <- df

### one species
lm1<- lm(area~Amoxicillin+Chlorothalonil+Diflufenican+Glyphosate+Imidacloprid+Metaldehyde+Oxytetracycline+Tebuconazole, data=lm.data)
lm1.5 <- lm(update.formula(lm1, .~.-Chlorothalonil-Tebuconazole), data=lm.data)
## save

### two species
lm2<- lm(area~Amoxicillin+Chlorothalonil+Diflufenican+Glyphosate+Imidacloprid+Metaldehyde+Oxytetracycline+Tebuconazole 
         + Amoxicillin:Chlorothalonil + Amoxicillin:Diflufenican + Amoxicillin:Glyphosate + Amoxicillin:Imidacloprid + Amoxicillin:Metaldehyde + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
         + Chlorothalonil:Diflufenican + Chlorothalonil:Glyphosate + Chlorothalonil:Imidacloprid + Chlorothalonil:Metaldehyde + Chlorothalonil:Oxytetracycline + Chlorothalonil:Tebuconazole
         + Diflufenican:Glyphosate + Diflufenican:Imidacloprid + Diflufenican:Metaldehyde + Diflufenican:Oxytetracycline + Diflufenican:Tebuconazole
         + Glyphosate:Imidacloprid + Glyphosate:Metaldehyde + Glyphosate:Oxytetracycline + Glyphosate:Tebuconazole
         + Imidacloprid:Metaldehyde + Imidacloprid:Oxytetracycline + Imidacloprid:Tebuconazole
         + Metaldehyde:Oxytetracycline + Metaldehyde: Tebuconazole
         + Oxytetracycline:Tebuconazole ,data=lm.data)
lm2.5 <- lm(update.formula(lm2, .~.-Chlorothalonil-Diflufenican-Glyphosate-Imidacloprid-Metaldehyde-Tebuconazole
                           -Chlorothalonil:Diflufenican-Chlorothalonil:Glyphosate-Chlorothalonil:Imidacloprid-Chlorothalonil:Metaldehyde-Chlorothalonil:Tebuconazole
                           -Diflufenican:Glyphosate-Diflufenican:Imidacloprid-Diflufenican:Metaldehyde-Diflufenican:Tebuconazole
                           -Glyphosate:Imidacloprid-Glyphosate:Metaldehyde-Glyphosate:Tebuconazole
                           -Imidacloprid:Metaldehyde-Imidacloprid:Tebuconazole
                           -Metaldehyde:Tebuconazole), data=lm.data)
## save, only Amoxicillin and Oxytetracycline has significant effect, while all of their interaction with other chemicals are also significant

### three species
lm3<- lm(area~Amoxicillin+Oxytetracycline
         + Amoxicillin:Chlorothalonil + Amoxicillin:Diflufenican + Amoxicillin:Glyphosate + Amoxicillin:Imidacloprid + Amoxicillin:Metaldehyde + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
         + Chlorothalonil:Oxytetracycline + Diflufenican:Oxytetracycline + Glyphosate:Oxytetracycline + Imidacloprid:Oxytetracycline + Metaldehyde:Oxytetracycline + Oxytetracycline:Tebuconazole 
         +Amoxicillin:Chlorothalonil:Diflufenican+Amoxicillin:Chlorothalonil:Glyphosate+Amoxicillin:Chlorothalonil:Imidacloprid+Amoxicillin:Chlorothalonil:Metaldehyde+Amoxicillin:Chlorothalonil:Oxytetracycline+Amoxicillin:Chlorothalonil:Tebuconazole+Amoxicillin:Diflufenican:Glyphosate+Amoxicillin:Diflufenican:Imidacloprid+Amoxicillin:Diflufenican:Metaldehyde+Amoxicillin:Diflufenican:Oxytetracycline+Amoxicillin:Diflufenican:Tebuconazole
         +Amoxicillin:Glyphosate:Imidacloprid+Amoxicillin:Glyphosate:Metaldehyde+Amoxicillin:Glyphosate:Oxytetracycline+Amoxicillin:Glyphosate:Tebuconazole
         +Amoxicillin:Imidacloprid:Metaldehyde+Amoxicillin:Imidacloprid:Oxytetracycline+Amoxicillin:Imidacloprid:Tebuconazole
         +Amoxicillin:Metaldehyde:Oxytetracycline+Amoxicillin:Metaldehyde:Tebuconazole+Amoxicillin:Oxytetracycline:Tebuconazole
         +Chlorothalonil:Diflufenican:Glyphosate+Chlorothalonil:Diflufenican:Imidacloprid+Chlorothalonil:Diflufenican:Metaldehyde+Chlorothalonil:Diflufenican:Oxytetracycline+Chlorothalonil:Diflufenican:Tebuconazole+Chlorothalonil:Glyphosate:Imidacloprid+Chlorothalonil:Glyphosate:Metaldehyde+Chlorothalonil:Glyphosate:Oxytetracycline+Chlorothalonil:Glyphosate:Tebuconazole
         +Chlorothalonil:Imidacloprid:Metaldehyde+Chlorothalonil:Imidacloprid:Oxytetracycline+Chlorothalonil:Imidacloprid:Tebuconazole
         +Chlorothalonil:Metaldehyde:Oxytetracycline+Chlorothalonil:Metaldehyde:Tebuconazole+Chlorothalonil:Oxytetracycline:Tebuconazole+Diflufenican:Glyphosate:Imidacloprid+Diflufenican:Glyphosate:Metaldehyde+Diflufenican:Glyphosate:Oxytetracycline+Diflufenican:Glyphosate:Tebuconazole
         +Diflufenican:Imidacloprid:Metaldehyde+Diflufenican:Imidacloprid:Oxytetracycline+Diflufenican:Imidacloprid:Tebuconazole
         +Diflufenican:Metaldehyde:Oxytetracycline+Diflufenican:Metaldehyde:Tebuconazole+Diflufenican:Oxytetracycline:Tebuconazole
         +Glyphosate:Imidacloprid:Metaldehyde+Glyphosate:Imidacloprid:Oxytetracycline+Glyphosate:Imidacloprid:Tebuconazole
         +Glyphosate:Metaldehyde:Oxytetracycline+Glyphosate:Metaldehyde:Tebuconazole+Glyphosate:Oxytetracycline:Tebuconazole
         +Imidacloprid:Metaldehyde:Oxytetracycline+Imidacloprid:Metaldehyde:Tebuconazole+Imidacloprid:Oxytetracycline:Tebuconazole+Metaldehyde:Oxytetracycline:Tebuconazole
         ,data=lm.data)
lm3.5 <- lm(area~Amoxicillin+Oxytetracycline
            + Amoxicillin:Chlorothalonil + Amoxicillin:Diflufenican + Amoxicillin:Glyphosate + Amoxicillin:Imidacloprid + Amoxicillin:Metaldehyde + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
            + Chlorothalonil:Oxytetracycline + Diflufenican:Oxytetracycline + Glyphosate:Oxytetracycline + Imidacloprid:Oxytetracycline + Metaldehyde:Oxytetracycline + Oxytetracycline:Tebuconazole 
            + Amoxicillin:Oxytetracycline:Chlorothalonil + Amoxicillin:Oxytetracycline:Diflufenican + Amoxicillin:Oxytetracycline:Glyphosate + Amoxicillin:Oxytetracycline:Imidacloprid + Amoxicillin:Oxytetracycline:Metaldehyde + Amoxicillin:Oxytetracycline:Tebuconazole + Oxytetracycline:Diflufenican:Glyphosate + Oxytetracycline:Glyphosate:Imidacloprid 
            ,data=lm.data)
## save

### four species
lm4 <- lm(area~Amoxicillin+Oxytetracycline
          + Amoxicillin:Chlorothalonil + Amoxicillin:Diflufenican + Amoxicillin:Glyphosate + Amoxicillin:Imidacloprid + Amoxicillin:Metaldehyde + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
          + Chlorothalonil:Oxytetracycline + Diflufenican:Oxytetracycline + Glyphosate:Oxytetracycline + Imidacloprid:Oxytetracycline + Metaldehyde:Oxytetracycline + Oxytetracycline:Tebuconazole 
          + Amoxicillin:Oxytetracycline:Chlorothalonil + Amoxicillin:Oxytetracycline:Diflufenican + Amoxicillin:Oxytetracycline:Glyphosate + Amoxicillin:Oxytetracycline:Imidacloprid + Amoxicillin:Oxytetracycline:Metaldehyde + Amoxicillin:Oxytetracycline:Tebuconazole + Oxytetracycline:Diflufenican:Glyphosate + Oxytetracycline:Glyphosate:Imidacloprid 
          + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid + Amoxicillin:Chlorothalonil:Diflufenican:Metaldehyde + Amoxicillin:Chlorothalonil:Diflufenican:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid + Amoxicillin:Chlorothalonil:Glyphosate:Metaldehyde + Amoxicillin:Chlorothalonil:Glyphosate:Oxytetracycline + Amoxicillin:Chlorothalonil:Glyphosate:Tebuconazole + Amoxicillin:Chlorothalonil:Imidacloprid:Metaldehyde + Amoxicillin:Chlorothalonil:Imidacloprid:Oxytetracycline + Amoxicillin:Chlorothalonil:Imidacloprid:Tebuconazole + Amoxicillin:Chlorothalonil:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid + Amoxicillin:Diflufenican:Glyphosate:Metaldehyde + Amoxicillin:Diflufenican:Glyphosate:Oxytetracycline + Amoxicillin:Diflufenican:Glyphosate:Tebuconazole + Amoxicillin:Diflufenican:Imidacloprid:Metaldehyde + Amoxicillin:Diflufenican:Imidacloprid:Oxytetracycline + Amoxicillin:Diflufenican:Imidacloprid:Tebuconazole + Amoxicillin:Diflufenican:Metaldehyde:Oxytetracycline + Amoxicillin:Diflufenican:Metaldehyde:Tebuconazole + Amoxicillin:Diflufenican:Oxytetracycline:Tebuconazole + Amoxicillin:Glyphosate:Imidacloprid:Metaldehyde + Amoxicillin:Glyphosate:Imidacloprid:Oxytetracycline + Amoxicillin:Glyphosate:Imidacloprid:Tebuconazole + 
          + Amoxicillin:Glyphosate:Metaldehyde:Oxytetracycline + Amoxicillin:Glyphosate:Metaldehyde:Tebuconazole + Amoxicillin:Glyphosate:Oxytetracycline:Tebuconazole + Amoxicillin:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid + Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde + Chlorothalonil:Diflufenican:Glyphosate:Oxytetracycline + Chlorothalonil:Diflufenican:Glyphosate:Tebuconazole + Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde + Chlorothalonil:Diflufenican:Imidacloprid:Oxytetracycline + Chlorothalonil:Diflufenican:Imidacloprid:Tebuconazole + Chlorothalonil:Diflufenican:Metaldehyde:Oxytetracycline + Chlorothalonil:Diflufenican:Metaldehyde:Tebuconazole + Chlorothalonil:Diflufenican:Oxytetracycline:Tebuconazole + Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde + Chlorothalonil:Glyphosate:Imidacloprid:Oxytetracycline + Chlorothalonil:Glyphosate:Imidacloprid:Tebuconazole + Chlorothalonil:Glyphosate:Metaldehyde:Oxytetracycline + Chlorothalonil:Glyphosate:Metaldehyde:Tebuconazole + Chlorothalonil:Glyphosate:Oxytetracycline:Tebuconazole + Chlorothalonil:Imidacloprid:Metaldehyde:Oxytetracycline + Chlorothalonil:Imidacloprid:Metaldehyde:Tebuconazole + Chlorothalonil:Imidacloprid:Oxytetracycline:Tebuconazole + Chlorothalonil:Metaldehyde:Oxytetracycline:Tebuconazole + Diflufenican:Glyphosate:Imidacloprid:Metaldehyde + Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline + Diflufenican:Glyphosate:Imidacloprid:Tebuconazole + Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline + Diflufenican:Glyphosate:Metaldehyde:Tebuconazole + Diflufenican:Glyphosate:Oxytetracycline:Tebuconazole + Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline + Diflufenican:Imidacloprid:Metaldehyde:Tebuconazole + Diflufenican:Imidacloprid:Oxytetracycline:Tebuconazole + Diflufenican:Metaldehyde:Oxytetracycline:Tebuconazole + Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole
          ,data=lm.data) 
lm4.5 <- lm(area~Amoxicillin+Oxytetracycline
            + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
            + Chlorothalonil:Oxytetracycline + Diflufenican:Oxytetracycline + Glyphosate:Oxytetracycline + Imidacloprid:Oxytetracycline + Metaldehyde:Oxytetracycline + Oxytetracycline:Tebuconazole
            + Amoxicillin:Oxytetracycline:Chlorothalonil + Amoxicillin:Oxytetracycline:Diflufenican + Amoxicillin:Oxytetracycline:Glyphosate + Amoxicillin:Oxytetracycline:Imidacloprid + Amoxicillin:Oxytetracycline:Metaldehyde
            ,data=lm.data)

### five species
lm5 <- lm(area~Amoxicillin+Oxytetracycline
          + Amoxicillin:Oxytetracycline + Amoxicillin:Tebuconazole
          + Chlorothalonil:Oxytetracycline + Diflufenican:Oxytetracycline + Glyphosate:Oxytetracycline + Imidacloprid:Oxytetracycline + Metaldehyde:Oxytetracycline + Oxytetracycline:Tebuconazole
          + Amoxicillin:Oxytetracycline:Chlorothalonil + Amoxicillin:Oxytetracycline:Diflufenican + Amoxicillin:Oxytetracycline:Glyphosate + Amoxicillin:Oxytetracycline:Imidacloprid + Amoxicillin:Oxytetracycline:Metaldehyde
          + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Oxytetracycline + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Glyphosate:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline + Amoxicillin:Diflufenican:Glyphosate:Metaldehyde:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Diflufenican:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Diflufenican:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline + Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline + Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Tebuconazole + Chlorothalonil:Diflufenican:Imidacloprid:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Chlorothalonil:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Chlorothalonil:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole
          ,data=lm.data)
## all five species interactions are not significant, so six species continues with lm4.5

### six species
lm6 <- lm(update.formula(lm4.5, .~. + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole), data=lm.data) 
## all six species interactions are not significant, so six species continues with lm4.5

### seven species + eight species
lm7 <- lm(update.formula(lm4.5, .~.+ Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole), data=lm.data)
lm7.5 <- lm(update.formula(lm4.5, .~. + Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole + Amoxicillin:Chlorothalonil:Diflufenican:Glyphosate:Imidacloprid:Metaldehyde:Oxytetracycline:Tebuconazole), data=lm.data)

### all included
lm8 <-lm(area~Amoxicillin*Chlorothalonil*Diflufenican*Glyphosate*Imidacloprid*Metaldehyde*Oxytetracycline*Tebuconazole, data=lm.data)

modelist

summary(lm8)
anova(lm8, lm7.5)

#### as factor
lm.data <- df
for(column in 8:16){
lm.data[[column]]<- as.factor(lm.data[[column]])
}


### is it required to set presence/absence as categorical variable?
lm0 <- lm(area~Amoxicillin, data=lm.data)
lm0.1 <- lm(area~Amoxicillin, data=lm.data)
lm8.1 <-lm(area~Amoxicillin*Chlorothalonil*Diflufenican*Glyphosate*Imidacloprid*Metaldehyde*Oxytetracycline*Tebuconazole, data=lm.data)
# compare this with the one above
summary(lm0)
summary(lm8.1)

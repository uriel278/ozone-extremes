library(here)
library(ismev)
library(INLA)
library(ggplot2)
library(tidyr)
library(tmap)
library(sf)

#Data mas limpia
polygon <- st_read(here('data', 'processed', 'small-polygon', 'small-polygon.shp'))
polygon <- st_transform(polygon, st_crs(projMercator))
mmax <- st_read(
  here('data', 'processed', 'mmax', 'mmax.shp')
)
mmax <- mmax %>%
  group_by(lnlt_nd) %>%
  arrange(year, month, .by_group = TRUE) %>%
  mutate("i_ar" = row_number()) %>%
  ungroup()
projMercator<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"
small_grid <- mmax %>%
  filter(year == 2013, month == 1) %>%
  select(lnlt_nd, geometry)

mmax%>%
  ggplot() +
  geom_line(aes(x = i_ar, y = O3, group = lnlt_nd), alpha = 0.03) +
  xlab("Time (Months)") +
  theme_bw()


# Calcula seasonality
#Using ismev to remove the seasonality and to center the data
cells <- unique(mmax$lnlt_nd)
month_x <- rep(1:12, 10)
#Matrix of covariates for seasonality
m_cell <- cbind(sin(2*1*pi*(month_x-1)/12),
                cos(2*1*pi*(month_x-1)/12),
                sin(2*2*pi*(month_x-1)/12),
                cos(2*2*pi*(month_x-1)/12))

nllh_matrix <- matrix(nrow = length(cells), ncol = 4)
xi_matrix <- matrix(nrow = length(cells), ncol = 4)
for(j in 1:length(cells)){
  #id of K cell
  id_j <- mmax$lnlt_nd == cells[j]
  #Get data for cell
  data_cell <- mmax[id_j,]
  y_cell <- (data_cell$year-2013)/100
  #Fit the first model and extract information
  model1_cell <- gev.fit(xdat = data_cell$O3, ydat = matrix(y_cell, ncol = 1), mul = 1)
  nllh_matrix[j, 1] <- model1_cell$nllh
  xi_matrix[j, 1] <- model1_cell$mle[4]
  #Fit the second model and extract information
  model2_cell <- gev.fit(xdat = data_cell$O3, ydat = cbind(y_cell, m_cell), mul = 1:3)
  nllh_matrix[j, 2] <- model2_cell$nllh
  xi_matrix[j, 2] <- model2_cell$mle[6]
  #Fit the third model and extract information
  model3_cell <- gev.fit(xdat = data_cell$O3, ydat = cbind(y_cell, m_cell), mul = 1:5)
  nllh_matrix[j, 3] <- model3_cell$nllh
  xi_matrix[j, 3] <- model3_cell$mle[8]
  #Fit the fourth model and extract information
  model4_cell <- gev.fit(xdat = data_cell$O3, ydat = cbind(y_cell, m_cell), mul = 1:5, sigl = 1, siglink = exp)
  nllh_matrix[j, 4] <- model4_cell$nllh
  xi_matrix[j, 4] <- model4_cell$mle[9]
}


#Nllh data
nllh_matrix <- cbind(cells, nllh_matrix)
head(nllh_matrix)
nllh_info <- as.data.frame(nllh_matrix) %>%
  pivot_longer(cols = 2:5, names_to = "Model", values_to = "Nllh", 
               names_prefix = "V")
nllh_info <- nllh_info %>%
  mutate("Model" = as.numeric(Model) - 1)
names(nllh_info)[1] <- "lnlt_nd"



nllh_info <- left_join(nllh_info, small_grid, by = "lnlt_nd")
nllh_info$Nllh <- as.numeric(nllh_info$Nllh)
nllh_info <- st_as_sf(nllh_info)
head(nllh_info)

xi_matrix <- cbind(cells, xi_matrix)
xi_info <- as.data.frame(xi_matrix) %>%
  pivot_longer(cols = 2:5, names_to = "Model", 
               names_prefix = "V", values_to = "Shape")
xi_info <- xi_info %>%
  mutate("Model" = as.numeric(Model) - 1,
         "Shape" = as.numeric(Shape))
names(xi_info)[1] <- "lnlt_nd"
xi_info <- left_join(xi_info, small_grid, by = "lnlt_nd")
xi_info <- st_as_sf(xi_info)

aic_info <- nllh_info
aic_info$aic <- NA
aic_info$aic[aic_info$Model == 1] <- 2*4 + 2*aic_info$Nllh[aic_info$Model == 1]
aic_info$aic[aic_info$Model == 2] <- 2*6 + 2*aic_info$Nllh[aic_info$Model == 2]
aic_info$aic[aic_info$Model == 3] <- 2*8 + 2*aic_info$Nllh[aic_info$Model == 3]
aic_info$aic[aic_info$Model == 4] <- 2*9 + 2*aic_info$Nllh[aic_info$Model == 4]

ggplot() +
  geom_sf(data = nllh_info, aes(fill = Nllh)) +
  facet_wrap("Model") +
  theme_bw()

ggplot() +
  geom_sf(data = xi_info, aes(fill = Shape)) +
  facet_wrap("Model") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)+
  theme_bw()
ggplot() +
  geom_sf(data = aic_info, aes(fill = aic)) +
  facet_wrap("Model") +
  scale_fill_distiller(palette = "YlOrRd")+
  labs(fill = "AIC")+
  theme_bw()


#This will be the model
mmax$O3_tilde <- NA
mmax$seasonality <- NA
for(k in 1:length(cells)){
  #id of K cell
  id_k <- mmax$lnlt_nd == cells[k]
  #Get data for cell
  data_cell <- mmax[id_k,]
  y_cell <- (data_cell$year-2013)/100
  #Fit the model
  model_cell3 <- gev.fit(xdat = data_cell$O3, ydat = cbind(y_cell, m_cell), mul = 1:5)
  #Correct the observations
  mmax$seasonality[id_k] <- model_cell3$vals[,1]
  mmax$O3_tilde[id_k] <- data_cell$O3 - model_cell3$vals[,1]
}


seasonality_correction <- mmax %>%
  group_by(i_ar) %>%
  summarise("s" = mean(seasonality))

mmax$snlt <- rep(seasonality_correction$s, 193)


mmax %>%
  ggplot() +
  geom_line(aes(x = i_ar, y = O3, group = lnlt_nd), alpha = 0.1) +
  geom_line(aes(x = i_ar, y = snlt, group = lnlt_nd, col ="red"), alpha = 0.1)+
  xlab("Time") + ylab("O3") +
  theme_bw()

mmax %>%
  ggplot() +
  geom_line(aes(x = i_ar, y = O3 - snlt, group = lnlt_nd), alpha = 0.1) +
  xlab("Time") + ylab("O3 - Season corrected") +
  theme_bw()

#Plot of the months of interest

mmax %>%
  filter(year == 2018, month >= 4, month <= 6) %>%
  tm_shape() +
  tm_fill(fill = "O3", fill.scale = tm_scale(values = "YlOrRd", n = 10))+
  tm_facets_wrap(by = "month", nrow = 1, scale.factor = 1) +
  tm_legend(orientation = "landscape",
            position = tm_pos_out("center", "bottom", pos.h = "center"),
            frame = FALSE, title = "Ozone level")+
  tm_scale_continuous()

#Define the model of interest
mesh_model <- inla.mesh.2d(
  loc.domain = polygon, 
  max.edge = c(15, 40), cutoff = 3.5, crs = st_crs(projMercator))
mesh_model$n
nodes_sf <- st_sfc(apply(mesh_model$loc[,-3], 1, st_point, simplify = F), crs = projMercator)
plot(mesh_model)
spde_model <- inla.spde2.matern(mesh = mesh_model, alpha = 2)
ggplot() +
  geom_sf(data = small_grid, aes(fill = NULL))+
  geom_sf(data = nodes_sf, size = 0.1) +
  theme_bw()

#Replace the polygon geometry by points
mmax_pt <- mmax
st_geometry(mmax_pt) <- st_centroid(mmax_pt$geometry)

################################################################################
################################################################################


#Index for replicates
spde_index <- inla.spde.make.index(name = "s",
                                   n.spde = spde_model$n.spde,
                                   n.group = 120)
mmax_pt$rep_id <- rep(1:120, 193)
#Create matrix A
Asr <- inla.spde.make.A(mesh = mesh_model,
                        loc = st_coordinates(mmax_pt),
                        group = mmax_pt$rep_id,
                        n.group = 120)
#Create stack to fit
null_mat1 <- matrix(NA, nrow = nrow(mmax_pt), ncol = 0)
spread.x1 <- null_mat1
tail.x1 <- null_mat1
stack_fit_sr <- inla.stack(
  data = inla.mdata(mmax_pt$O3 - mmax_pt$snlt, spread.x1, tail.x1),
  A = list(Asr),
  effects = list(spde_index),
  tag= "fit_sr"
)
#Base model
f1 <- inla.mdata(Y1, spread.x1, tail.x1) ~ f(s, model = spde_model, 
                                             group = s.group, 
                                             control.group = list(model = "iid"))
fit1 <- inla(formula = f1, family = "bgev",
             data = inla.stack.data(stack_fit_sr),
             control.predictor = list(A = inla.stack.A(stack_fit_sr), 
                                      compute = TRUE,
                                      link = 1),
             control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
             verbose = TRUE)
summary(fit1)
saveRDS(fit1, here('outputs', 'fitted-models', 'fit1.rds'))
fit1_prev <- fit1

#Get the mesh nodes that are inside each cell-grid
points_in_cells <- st_contains(small_grid, nodes_sf)
locin <- mesh_model$loc[unlist(points_in_cells),-3]
block <- NULL
for(i in 1:length(points_in_cells)){
  block <- c(block, rep(i, length(points_in_cells[[i]])))  
}
mmax$rep_id <- rep(1:120, 193)
#Define the projection matrix for aggregation and projection
As_d <- inla.spde.make.A(mesh = mesh_model,
                         loc = locin, 
                         block = block,
                         block.rescale="sum")
As_dd <- kronecker(diag(1, 120), As_d)
dim(As_dd)

#Create stack to fit
null_mat1_d <- matrix(NA, nrow = nrow(mmax), ncol = 0)
spread.x1_d <- null_mat1_d
tail.x1_d <- null_mat1_d
stack_fit_sr_d <- inla.stack(
  data = inla.mdata(mmax$O3 - mmax$snlt, spread.x1_d, tail.x1_d),
  A = list(As_dd),
  effects = list(spde_index),
  tag= "fit_sr"
)
#Trying to fit the model
f1_d <- inla.mdata(Y1, spread.x1_d, tail.x1_d) ~ f(s, model = spde_model, 
                                                   group = s.group, 
                                                   control.group = list(model = "iid"))
fit1_d <- inla(formula = f1_d, family = "bgev",
               data = inla.stack.data(stack_fit_sr_d),
               control.predictor = list(A = inla.stack.A(stack_fit_sr_d), 
                                        compute = TRUE,
                                        link = 1),
               control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
               control.mode = list(
                 result = fit1_prev,
                 restart = TRUE),
               verbose = TRUE)
summary(fit1_d)
saveRDS(fit1_d, here('outputs', 'fitted-models', 'fit1_d.rds'))

#Posterior parameters of bGEV
par(mfrow=c(1,2))
plot(inla.smarginal(marginal = fit1_d$marginals.hyperpar$`spread for BGEV observations`), 
     type = "l",
     xlab = "Value", ylab = "Density", main = "Posterior: Spread",
     cex.main = 0.7)
plot(inla.smarginal(marginal = fit1_d$marginals.hyperpar$`tail for BGEV observations`), 
     type = "l",
     xlab = "Value", ylab = "Density", main = "Posterior: Tail",
     cex.main = 0.7)
par(mfrow=c(1,1))
#Posterior parameters for the spatial component
par(mfrow=c(1,2))
plot(inla.tmarginal(exp, marginal = fit1_d$marginals.hyperpar$`Theta1 for s`), 
     type = "l", xlab = "Value", ylab = "Density", 
     main = "Posterior: Range",
     cex.main = 0.7)
plot(inla.tmarginal(exp, marginal = fit1_d$marginals.hyperpar$`Theta2 for s`), 
     type = "l", xlab = "Value", ylab = "", 
     main = "Posterior: Marginal precision scaling",
     cex.main = 0.7)
par(mfrow=c(1,1))

grid_pred <- st_make_grid(
  polygon,
  cellsize = 5,         # example: 1 km grid cells
  what = "polygons",
  square = TRUE
)
grid_pred <- st_intersection(grid_pred, polygon)
ggplot() +
  geom_sf(data = polygon, aes(fill = NA)) +
  geom_sf(data = grid_pred, aes(fill = NA))
#### Draw posterior samples
post_samples <- inla.posterior.sample(n = 300, result = fit1_d)
##### Simulating the field
sim_fields <- matrix(NA, nrow = 1811, ncol = 300)
Apred <- inla.spde.make.A(mesh = mesh_model, loc = st_coordinates(st_centroid(grid_pred)))
for(k in 1:300){
  Q_trial <- inla.spde.precision(spde = spde_model, theta = post_samples[[k]]$hyperpar[3:4])
  sim_field[,k] <- Apred%*%inla.qsample(n = 1, Q = Q_trial)
}


s_returns <- seasonality_correction$s[12*5+c(4,5,6)]

compute_return <- function(mu, sigma, xi, p){
  zp <- mu + (sigma/xi)*(1-(-log(1-p))^(-xi))
  return(zp)
}
giveme.gev.par = function(q, sbeta, alpha, beta, xi)
{
  #From bgev documentation
  .mu = function(q, sbeta, alpha, beta, xi) {
    a =-log(1-beta/2)
    b =-log(beta/2)
    c =-log(alpha)
    if (all(xi > 0.0)) {
      tmp0 = (c^(-xi)- 1)/xi
      tmp1 = a^(-xi)
      tmp2 = b^(-xi)
      dbeta = (tmp1- tmp2)/xi
      return(q- (sbeta/dbeta) * tmp0)
    } else if (all(xi== 0.0)) {
      dbeta = log(b)- log(a)
      tmp0 = log(c)
      return(q + (sbeta/dbeta) * tmp0)
    } else {
      stop("mixed case not implemented")
    }
  }
  .sigma = function(q, sbeta, alpha, beta, xi) {
    a =-log(1-beta/2)
    b =-log(beta/2)
    if (all(xi > 0.0)) {
      tmp1 = a^(-xi)
      tmp2 = b^(-xi)
      dbeta = (tmp1- tmp2)/xi
      return(sbeta/dbeta)
    } else if (all(xi== 0.0)) {
      dbeta = log(b)- log(a)
      return(sbeta/dbeta)
    } else {
      stop("mixed case not implemented")
    }
  }
  return(list(mu = .mu(q, sbeta, alpha, beta, xi),
              sigma = .sigma(q, sbeta, alpha, beta, xi),
              xi = xi))
}

# ret_levels <- matrix(NA, nrow = 1811, ncol = 300)
ret_1804 <- matrix(NA, nrow = 1811, ncol = 300)
ret_1805 <- matrix(NA, nrow = 1811, ncol = 300)
ret_1806 <- matrix(NA, nrow = 1811, ncol = 300)
for(k in 1:300){
  for(j in 1:1811){
    gev_pars_kj <- giveme.gev.par(q = sim_field[j,k],
                                  sbeta = post_samples[[k]]$hyperpar[1], 
                                  alpha = 0.5, beta = 0.25, 
                                  xi = post_samples[[k]]$hyperpar[2])
    ret_1804[j,k] <- compute_return(mu = gev_pars_kj$mu + s_returns[1],
                                      sigma = gev_pars_kj$sigma,
                                      xi = gev_pars_kj$xi,
                                      p = 1/(12*50))
    ret_1805[j,k] <- compute_return(mu = gev_pars_kj$mu + s_returns[2],
                                    sigma = gev_pars_kj$sigma,
                                    xi = gev_pars_kj$xi,
                                    p = 1/(12*50))
    ret_1805[j,k] <- compute_return(mu = gev_pars_kj$mu + s_returns[3],
                                    sigma = gev_pars_kj$sigma,
                                    xi = gev_pars_kj$xi,
                                    p = 1/(12*50))
  }
}

quantiles_1804 <- quantiles_ret_levels <- apply(ret_1804, 1, FUN = function(r) quantile(r, probs = c(0.05/2, 0.5, 1-0.05/2)))
quantiles_1804 <- t(quantiles_1804)
quantiles_1805 <- quantiles_ret_levels <- apply(ret_1805, 1, FUN = function(r) quantile(r, probs = c(0.05/2, 0.5, 1-0.05/2)))
quantiles_1805 <- t(quantiles_1805)
quantiles_1806 <- quantiles_ret_levels <- apply(ret_1806, 1, FUN = function(r) quantile(r, probs = c(0.05/2, 0.5, 1-0.05/2)))
quantiles_1806 <- t(quantiles_1806)


rl_1804_sf <- st_sf(quantiles_1804, geometry = grid_pred)
rl_1804_sf <- 
  rl_1804_sf %>%
  pivot_longer(cols = 1:3, names_to = "level", values_to = "Return")

rl_1805_sf <- st_sf(quantiles_1805, geometry = grid_pred)
rl_1805_sf <- 
  rl_1805_sf %>%
  pivot_longer(cols = 1:3, names_to = "level", values_to = "Return") 

rl_1806_sf <- st_sf(quantiles_1806, geometry = grid_pred)
rl_1806_sf <- 
  rl_1806_sf %>%
  pivot_longer(cols = 1:3, names_to = "level", values_to = "Return") 


rl_1804_sf %>%
  tm_shape() +
  tm_fill(fill = "Return", fill.scale = tm_scale(values = "YlOrRd", n = 10))+
  tm_facets_wrap(by = "level", nrow = 1, scale.factor = 1) +
  tm_legend(orientation = "landscape",
            position = tm_pos_out("center", "bottom", pos.h = "center"),
            frame = FALSE, title = "Ozone return level")+
  tm_scale_continuous()

rl_1805_sf %>%
  tm_shape() +
  tm_fill(fill = "Return", fill.scale = tm_scale(values = "YlOrRd", n = 10))+
  tm_facets_wrap(by = "level", nrow = 1, scale.factor = 1) +
  tm_legend(orientation = "landscape",
            position = tm_pos_out("center", "bottom", pos.h = "center"),
            frame = FALSE, title = "Ozone return level")+
  tm_scale_continuous()

rl_1806_sf %>%
  tm_shape() +
  tm_fill(fill = "Return", fill.scale = tm_scale(values = "YlOrRd", n = 10))+
  tm_facets_wrap(by = "level", nrow = 1, scale.factor = 1) +
  tm_legend(orientation = "landscape",
            position = tm_pos_out("center", "bottom", pos.h = "center"),
            frame = FALSE, title = "Ozone return level")+
  tm_scale_continuous()
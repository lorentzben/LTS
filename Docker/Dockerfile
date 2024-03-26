FROM rocker/verse:4.1.0
LABEL authors="Ben Lorentz"
RUN apt-get update -y && apt-get install -y  git make   libmariadbclient-dev libmariadb-dev libicu-dev  pandoc  libfreetype6-dev  libglu1-mesa-dev  libpng-dev  libgl1-mesa-dev  zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
#CMD ["bash"]
CMD ["R"]
ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=2022.02.2+485
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
RUN /bin/sh -c /rocker_scripts/install_rstudio.sh # buildkit
RUN /bin/sh -c /rocker_scripts/install_pandoc.sh # buildkit
CMD ["/init"]

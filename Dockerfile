FROM rocker/verse:4.2.3

COPY ./utils.R ./utils.R

# libgsl-dev is required for RcppZiggurat
RUN apt-get update && \
    apt-get install -y --no-install-recommends libgsl-dev

RUN Rscript utils.R

RUN Rscript -e "install.packages(c('tinytex'), repos = 'http://cran.rstudio.com/')"
RUN if [ "$(uname -m)" = "x86_64" ]; then \
      Rscript -e 'tinytex::install_tinytex(force = TRUE, version = "2023.04")'; \
    else \
      Rscript -e 'tinytex::install_tinytex(force = TRUE)'; \
    fi

ENV PATH "/root/.TinyTeX/bin/x86_64-linux/:${PATH}"

RUN tlmgr update --self --all && \
  tlmgr install fancyhdr cleveref multirow listings \
  xcolor grffile titling amsmath kvsetkeys etoolbox \
  pdftexcmds infwarerr geometry fancyvrb framed booktabs \
  mdwtools epstopdf-pkg kvoptions ltxcmds auxhook bigintcalc \
  bitset etexcmds gettitlestring hycolor hyperref intcalc kvdefinekeys \
  letltxmacro pdfescape refcount rerunfilecheck stringenc uniquecounter \
  zapfding pgf caption enumitem textpos mdframed zref needspace mdframed \
  csquotes wrapfig colortbl pdflscape varwidth threeparttable threeparttablex \
  environ trimspaces ulem makecell biblatex logreq biber tabu
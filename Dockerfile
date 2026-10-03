FROM rocker/r-ver:4.6.1
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev libssl-dev libxml2-dev && rm -rf /var/lib/apt/lists/*
WORKDIR /srv/fieldnotes
COPY renv.lock renv.lock
RUN R -e 'install.packages("renv", repos="https://cloud.r-project.org"); renv::restore(lockfile="renv.lock", library="/usr/local/lib/R/site-library", prompt=FALSE)'
COPY app.R run.R DESCRIPTION ./
COPY R/ R/
COPY www/ www/
COPY scenarios/ scenarios/
COPY starter/ starter/
RUN useradd --create-home fieldnotes && chown -R fieldnotes:fieldnotes /srv/fieldnotes
USER fieldnotes
ENV FIELDNOTES_HOST=0.0.0.0 PORT=3838
EXPOSE 3838
CMD ["Rscript", "run.R"]

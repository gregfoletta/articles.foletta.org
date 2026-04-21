FROM rocker/r-ver:4.4

# System deps: tidyverse runtime libs + CmdStan C++ toolchain + Node.js
# Using rocker/r-ver (no RStudio Server) to avoid the sg-rstudio orphan
# process that caused containers to hang in rocker/tidyverse.
RUN apt-get update && apt-get install -y --no-install-recommends \
    # tidyverse / ggplot2 / ragg dependencies
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Node.js setup
    curl \
    ca-certificates \
    gnupg \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key \
        | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_22.x nodistro main" \
        > /etc/apt/sources.list.d/nodesource.list \
    && apt-get update && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

# R packages via Posit Package Manager (precompiled binaries — much faster
# than CRAN source builds for tidyverse on Linux).
ENV PPM=https://packagemanager.posit.co/cran/__linux__/noble/latest
RUN Rscript -e "\
    install.packages( \
        c('tidyverse', 'jsonlite', 'posterior', 'tidybayes'), \
        repos = '${PPM}', quiet = TRUE); \
    install.packages('cmdstanr', \
        repos = c('https://stan-dev.r-universe.dev', '${PPM}'), \
        quiet = TRUE) \
"

# Install CmdStan into /opt/
RUN Rscript -e "cmdstanr::install_cmdstan(dir = '/opt', cores = 4, quiet = FALSE)"
# Write the versioned CmdStan path into the global R profile
RUN echo "cmdstanr::set_cmdstan_path('$(ls -d /opt/cmdstan-*)')" \
    >> /usr/local/lib/R/etc/Rprofile.site

# Claude Code CLI
RUN npm install -g @anthropic-ai/claude-code

# Non-root user for safer execution
RUN useradd -m -u 1001 analyst
USER analyst

WORKDIR /work

ENTRYPOINT ["/bin/bash"]

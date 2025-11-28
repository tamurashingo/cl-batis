FROM fukamachi/sbcl

RUN apt-get update && apt-get install -y \
  default-libmysqlclient-dev \
  libpq-dev \
  libsqlite3-dev \
  default-mysql-client \
  postgresql-client

RUN ros install rove

ENV PATH=${PATH}:/root/.roswell/bin
ENV CL_SOURCE_REGISTRY=/app

RUN mkdir /volumes
RUN mkdir /app
WORKDIR /app

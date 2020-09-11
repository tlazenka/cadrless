FROM clojure:openjdk-8-lein-2.9.1

RUN curl -O https://download.clojure.org/install/linux-install-1.10.1.536.sh
RUN chmod +x linux-install-1.10.1.536.sh
RUN ./linux-install-1.10.1.536.sh

ENV APP_HOME /app
WORKDIR $APP_HOME

COPY ./project.clj .
RUN lein deps

COPY . .

CMD ["lein", "test"]
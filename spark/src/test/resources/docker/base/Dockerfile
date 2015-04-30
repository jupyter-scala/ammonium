FROM dockerfile/java:oracle-java8
MAINTAINER Alexandre Archambault <alexandre.archambault@gmail.com>


RUN curl -s http://d3kbcqa49mib13.cloudfront.net/spark-1.3.1-bin-hadoop2.6.tgz | tar -xz -C /opt/
RUN curl -s http://d3kbcqa49mib13.cloudfront.net/spark-1.2.2-bin-hadoop2.4.tgz | tar -xz -C /opt/
RUN curl -s http://d3kbcqa49mib13.cloudfront.net/spark-1.1.1-bin-hadoop2.4.tgz | tar -xz -C /opt/
RUN cd /opt && ln -s spark-1.3.1-bin-hadoop2.6 spark-1.3 && ln -s spark-1.2.2-bin-hadoop2.4 spark-1.2 && ln -s spark-1.1.1-bin-hadoop2.4 spark-1.1

ENV SPARK_HOME /opt/spark-1.3

WORKDIR /root

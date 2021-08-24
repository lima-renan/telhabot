FROM debian

RUN apt-get update && apt-get upgrade -y

# Set the locale
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y locales

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8

ENV LANG en_US.UTF-8 


RUN apt-get install -y \
       build-essential \
       nano \
        curl \
        unzip \
        git \
        vim \
        wget \
        libtinfo-dev \
        libpq-dev
        
RUN wget -qO- https://get.haskellstack.org/ | sh -y


# for run Dockerfile

#docker build -t mydebian .

#after run Docker file:

#install haskell stack
#docker run --name mydebian -it mydebian 

#wget -qO- https://get.haskellstack.org/ | sh

#to copy project
#docker cp habot/ mydebian:/home

#to run project in Docker
#stack run --allow-different-user




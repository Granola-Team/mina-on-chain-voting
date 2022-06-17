resource "aws_security_group" "mina_sg" {
  name        = "mina_sg"
  description = "Allow traffic for the mina node"

  ingress {
    description      = "SSH"
    from_port        = 22
    to_port          = 22
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  ingress {
    description      = "mina1"
    from_port        = 8302
    to_port          = 8302
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  ingress {
    description      = "TLS"
    from_port        = 443
    to_port          = 443
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  ingress {
    description      = "HTTP"
    from_port        = 80
    to_port          = 80
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

   ingress {
    description      = "mina2"
    from_port        = 3085
    to_port          = 3085
    protocol         = "tcp"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  egress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  tags = {
    Name = "mina_sg"
  }
}

resource "aws_instance" "node" {
  ami           = "ami-0cb4e786f15603b0d"
  instance_type = "c5.2xlarge"
  key_name = "shailesh_mina"
  security_groups = [aws_security_group.mina_sg.name]

  tags = {
    Name = "minanode"
  }
}

#Bootstrap ansible
locals {
  user_data = <<EOF
#cloud-config
packages:
  - python3
  - python3-dev
  - python3-setuptools
EOF
}

provider "aws" {
  region = var.region
}

resource "aws_s3_bucket" "log_bucket" {
  bucket = "espanelm-website-log"
  acl    = "log-delivery-write"

  tags = {
    Name = var.name_tag
  }
}

resource "aws_s3_bucket" "espanelm" {
  bucket = var.app_bucket_name
  acl    = "public-read"

  tags = {
    Name = var.name_tag
  }

  policy = <<EOF
{
    "Version":"2008-10-17",
    "Statement":[{
    "Sid":"AllowPublicRead",
    "Effect":"Allow",
    "Principal": {"AWS": "*"},
    "Action":["s3:GetObject"],
    "Resource":["arn:aws:s3:::${var.app_bucket_name}/*"]
    }]
}
EOF

  logging {
    target_bucket = "${aws_s3_bucket.log_bucket.id}"
    target_prefix = "log/"
  }

  # error (404) points to index, so that
  # only the spa deals with routing
  website {
    index_document = "index.html"
    error_document = "index.html"
  }
}

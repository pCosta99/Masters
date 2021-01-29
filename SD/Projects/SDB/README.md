# [Mattermost](https://mattermost.com) - Systems Deployment and Benchmarking

Using [Ansible](https://ansible.com) we created a fully-automated deployment for Mattermost.
We used Google Cloud Platform to host our on-the-fly-generated VM's which makes it even more scalable.

The delivered setup consisted of:
- 1 Proxy that used NGINX with SSH protocol (using OpenSSH since we didn't used domains).
- 3 Databases. A master-slave architecture was used here with 1 master (for write operations) and 2 slaves (for read operations).
- 1 File Storage using [MinIO](https://min.io), a High Performance Object Storage compatible with Amazon S3.
- 2 Applicational Servers running Mattermost. The load was balanced in a round-robin fashion by the proxy.
- 1 Machine for Monitoring. It used techonologies such as MectricBeats, FileBeats, PacketBeat, ElasticSearch and Kibana.

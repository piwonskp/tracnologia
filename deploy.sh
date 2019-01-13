docker-compose run --rm frontend npm run build &&
    sudo rm -rf backend/frontend/ &&
    sudo mv frontend/build/ backend/frontend &&
    (cd backend; heroku container:push web -a tracnologia)

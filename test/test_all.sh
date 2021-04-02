# Runs all tests

echo "***********************************************"
echo "            STARTING ALL CHECKS!               "
echo "***********************************************"

./test/run_regression_tests.sh -a
if [ $? -ne 0 ]
then
    echo "AST Checks Failed!"
    exit 1
fi

./test/run_regression_tests.sh -s
if [ $? -ne 0 ]
then
    echo "Semantic Checks Failed!"
    exit 1
fi

./test/run_regression_tests.sh -c
if [ $? -ne 0 ]
then
    echo "Full Pipeline Checks Failed!"
    exit 1
fi

echo "***********************************************"
echo "              ALL CHECKS PASS!                 "
echo "***********************************************"
<?php

namespace LaraPackage\BehatApi;

use Behat\Behat\Context\Context;
use Behat\Behat\Context\SnippetAcceptingContext;
use Behat\Gherkin\Node\PyStringNode;
use Behat\MinkExtension\Context\MinkContext;
use Exception;
use GuzzleHttp\Client;
use GuzzleHttp\Exception\BadResponseException;
use OutOfBoundsException;
use OutOfRangeException;
use PHPUnit_Framework_Assert as PhpUnit;

/**
 * Defines application features from the specific context.
 */
class ApiFeatureContext extends MinkContext implements Context, SnippetAcceptingContext
{

    protected $apiVersion = 4;

    /**
     * The Guzzle HTTP Client.
     *
     * @var \GuzzleHttp\Client
     */
    protected $client;

    /**
     * The current HTTP method
     *
     * @var string
     */
    protected $httpMethod;

    /**
     * The current resource
     */
    protected $resource;

    /**
     * The request payload
     */
    protected $requestPayload;

    /**
     * The Guzzle HTTP Response.
     *
     * @var \Psr\Http\Message\ResponseInterface
     */
    protected $response;

    /**
     * The decoded response object.
     */
    protected $responsePayload;

    /**
     * The current scope within the response payload
     * which conditions are asserted against.
     * Uses OO style dot notation: data.0 would be $data[0]
     */
    protected $scope;

    /**
     * @var \LaraPackage\RandomId\Helper
     */
    protected $testingHelper;

    /**
     * @var int[]
     */
    protected $ids;

    /**
     * @var \PrometheusApi\Utilities\Uri\Parser
     */
    protected $resourceParser;

    /**
     * @var array
     */
    protected $headers = [];

    /**
     * @var string
     */
    protected $query;

    protected $guzzleConfig = [];

    protected $data = [];

    /**
     * Initializes context.
     * Every scenario gets it's own context object.
     *
     * @param array $parameters context parameters (set them up through behat.yml)
     */
    public function __construct($baseUri = null, $guzzle = [])
    {
        $config = [];
        if (null === $baseUri) {
            $config['base_uri'] = 'http://'.getenv('END_TO_END_TESTS_URI');
        } else {
            $config['base_uri'] = $baseUri;
        }

        $this->guzzleConfig = array_merge($config, $guzzle);

        $this->client = new Client();
        $this->resourceParser = \App::make(\PrometheusApi\Utilities\Contracts\Uri\Parser::class);
        $this->testingHelper = \App::make(\LaraPackage\RandomId\Helper::class);
    }

    /**
     * @Transform /^(\d+)$/
     */
    public function castStringToNumber($string)
    {
        return (int)$string;
    }

    /**
     * @Then /^I get a "([^"]*)" response$/
     */
    public function iGetAResponse($statusCode)
    {
        $response = $this->getResponse();
        $contentType = $response->getHeader('Content-Type')[0];

        if ($contentType === 'application/vnd.wps_api.v4+json') {
            $bodyOutput = $response->getBody();
        } else {
            $bodyOutput = 'Output is '.$contentType.', which is not JSON and is therefore scary. Run the request manually.';
        }

        PhpUnit::assertSame((int)$statusCode, (int)$this->getResponse()->getStatusCode(), $bodyOutput);
    }

    /**
     * @Given /^I have the file "([^"]*)"$/
     */
    public function iHaveTheFile($fileName)
    {
        $filePath = storage_path('/testing/').$fileName;
        $this->requestPayload = file_get_contents($filePath);
    }

    /**
     * @Given /^I have the payload:$/
     */
    public function iHaveThePayload(PyStringNode $requestPayload)
    {
        $this->requestPayload = $requestPayload;
    }

    /**
     * @Given /^I have the payload that needs ids:$/
     */
    public function iHaveThePayloadThatNeedsIds(PyStringNode $payload)
    {
        $decodedPayload = $this->decodePayload($payload);
        $columnIds = $this->testingHelper->getRandomIdsForPayload($decodedPayload);
        if ($columnIds) {
            $this->requestPayload = $this->testingHelper->putIdsInPayload($payload, $columnIds);
        } else {
            $columnIds = $this->testingHelper->getRandomIdsForLastEntity($this->resource, $payload);
            $this->requestPayload = $this->testingHelper->putIdsInPayload($payload, $columnIds);
        }
    }

    /**
     * @Given /^I have the payload that needs ids not in the pivot:$/
     */
    public function iHaveThePayloadThatNeedsIdsNotInThePivot(PyStringNode $requestPayload)
    {
        $idNeededCount = substr_count($requestPayload, '{random_id}');
        $ids = $this->testingHelper->getRandomIdsForLastEntityNotInPivot($this->resource, $idNeededCount);
        $this->requestPayload = $this->testingHelper->putIdsInPayload($requestPayload, $ids);
    }

    /**
     * @When /^I make the request$/
     */
    public function iMakeTheRequest()
    {
        $this->makeRequest();
    }

    /**
     * @When /^I request "(GET|PUT|POST|PATCH|DELETE) ([^"]*)"$/
     */
    public function iRequest($httpMethod, $uri)
    {
        $this->setResourceAndQuery($httpMethod, $uri);
        $this->putRandomIdsInResource();
        $this->makeRequest();
    }

    /**
     * @When /^I request "(GET|PUT|POST|PATCH|DELETE) ([^"]*)" and the query needs random data$/
     */
    public function iRequestAndTheQueryNeedsRandomData($httpMethod, $resource)
    {
        $this->setResourceAndQuery($httpMethod, $resource);
        $this->putRandomIdsInResource();
        $this->putRandomDataInQuery();
        $this->makeRequest();
    }

    /**
     * @When /^I request "(GET) ([^"]*)" without the previous ending id$/
     */
    public function iRequestWithoutThePreviousEndingId($httpMethod, $uri)
    {
        $this->setResourceAndQuery($httpMethod, $uri);

        $ids = $this->ids;
        array_pop($ids);
        $this->putRandomIdsInResource($ids, '{id}');
        $this->makeRequest();
    }

    /**
     * @Then /^I save the data$/
     */
    public function iSaveTheData()
    {
        $this->data = $this->getScopePayload();
    }

    /**
     * @Given /^I set the "([^"]*)" header with "([^"]*)"$/
     */
    public function iSetTheHeaderWith($header, $value)
    {
        $this->headers[$header] = $value;
    }

    /**
     * @Then /^I should not see the ids from the original request$/
     */
    public function iShouldNotSeeTheIdsFromTheOriginalRequest()
    {
        $responsePayload = $this->getScopePayload(true);
        $responseIds = $this->getIdsFromNestedArray($responsePayload);

        $ids = $this->ids;
        $lastIds = (array)array_pop($ids);

        foreach ($lastIds as $id) {
            PhpUnit::assertTrue(!in_array($id, $responseIds));
        }
    }

    /**
     * @Then /^I should see only the ids from the original payload$/
     */
    public function iShouldSeeOnlyTheIdsFromTheOriginalPayload()
    {
        $requestPayload = $this->decodePayload($this->requestPayload);
        $responsePayload = $this->getScopePayload(true);

        $requestIds = $this->getIdsFromNestedArray($requestPayload);
        $responseIds = $this->getIdsFromNestedArray($responsePayload);
        sort($requestIds);
        sort($responseIds);
        PhpUnit::assertSame($requestIds, $responseIds);
    }

    /**
     * @Then /^I should see the ids from the original payload$/
     */
    public function iShouldSeeTheIdsFromTheOriginalPayload()
    {
        $requestPayload = $this->decodePayload($this->requestPayload);
        $responsePayload = $this->getScopePayload(true);

        $requestIds = $this->getIdsFromNestedArray($requestPayload);
        $responseIds = $this->getIdsFromNestedArray($responsePayload);

        foreach ($requestIds as $id) {
            PhpUnit::assertTrue(in_array($id, $responseIds));
        }
    }

    /**
     * @Then /^I should see the properties and values from the original payload$/
     */
    public function iShouldSeeThePropertiesAndValuesFromTheOriginalPayload()
    {
        $requestPayload = $this->decodePayload($this->requestPayload);
        $responsePayload = $this->getScopePayload(true);

        $this->compareArrays($requestPayload, $responsePayload);
    }

    /**
     * @Then /^I should see these properties with their values from the original payload:$/
     */
    public function iShouldSeeThesePropertiesWithTheirValuesFromTheOriginalPayload(PyStringNode $string)
    {
        $requestPayload = $this->decodePayload($this->requestPayload);
        $responsePayload = $this->getScopePayload(true);

        $properties = array_filter(explode(PHP_EOL, $string));

        $filteredRequestPayload = array_intersect_key($requestPayload, array_flip($properties));

        $this->compareArrays($filteredRequestPayload, $responsePayload);
    }

    /**
     * @Given /^I use the "([^"]*)" from the response payload$/
     */
    public function iUseTheFromTheResponsePayload($property)
    {
        $this->ids = [$this->arrayGet($this->getScopePayload(), $property)];
    }

    /**
     * @Given /^I will request "(GET|PUT|POST|PATCH|DELETE) ([^"]*)"$/
     */
    public function iWillRequest($httpMethod, $resource)
    {
        $this->httpMethod = $httpMethod;
        $this->resource = $resource;
        $this->putRandomIdsInResource();
    }

    /**
     * @Given /^it is empty$/
     */
    public function itIsEmpty()
    {
        PhpUnit::assertEmpty($this->getScopePayload());
    }

    /**
     * @Given /^pick a random item from the "([^"]*)" property$/
     */
    public function pickARandomItemFromTheProperty($scope)
    {
        $this->maybeSetSubScope($scope);
        $randomArrayKey = array_rand($this->getScopePayload());
        $this->addToCurrentScope($randomArrayKey);
    }

    /**
     * @Then /^pick the first item from the "([^"]*)" property$/
     */
    public function pickTheFirstItemFromTheProperty($scope)
    {
        $this->maybeSetSubScope($scope);

        $payload = $this->getScopePayload();
        reset($payload);
        $firstKey = key($payload);

        $this->addToCurrentScope($firstKey);
    }

    /**
     * @Given /^reset scope$/
     */
    public function resetScope()
    {
        $this->scope = null;
    }

    /**
     * @Given /^scope into the first "([^"]*)" property$/
     */
    public function scopeIntoTheFirstProperty($scope)
    {
        $this->scope = "{$scope}.0";
    }

    /**
     * @Given /^scope into the "([^"]*)" property$/
     */
    public function scopeIntoTheProperty($scope)
    {
        $this->maybeSetSubScope($scope);
    }

    /**
     * @Given /^the properties exist:$/
     */
    public function thePropertiesExist(PyStringNode $propertiesString)
    {
        foreach (explode("\n", (string)$propertiesString) as $property) {
            $this->thePropertyExists($property);
        }
    }

    /**
     * @Given /^the "([^"]*)" property contains (\d+) items$/
     */
    public function thePropertyContainsItems($property, $count)
    {
        $payload = $this->getScopePayload();

        PhpUnit::assertCount(
            $count,
            $this->arrayGet($payload, $property),
            "Asserting the [$property] property contains [$count] items: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property equals "([^"]*)"$/
     */
    public function thePropertyEquals($property, $expectedValue)
    {
        $payload = $this->getScopePayload();
        $actualValue = $this->arrayGet($payload, $property);

        PhpUnit::assertEquals(
            $actualValue,
            $expectedValue,
            "Asserting the [$property] property in current scope equals [$expectedValue]: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property equals the random "([^"]*)"$/
     */
    public function thePropertyEqualsTheRandom($property, $randomKey)
    {
        $payload = $this->getScopePayload();

        $actual = $this->arrayGet($payload, $property);
        $expected = $this->getRandomDataForKey($randomKey);

        PhpUnit::assertSame($actual, $expected);
    }

    /**
     * @Given /^the "([^"]*)" property exists$/
     */
    public function thePropertyExists($property)
    {
        $payload = $this->getScopePayload();

        $message = sprintf(
            'Asserting the [%s] property exists in the scope [%s]: %s',
            $property,
            $this->scope,
            json_encode($payload)
        );

        if (is_object($payload)) {
            PhpUnit::assertTrue(array_key_exists($property, get_object_vars($payload)), $message);

        } else {
            PhpUnit::assertTrue(array_key_exists($property, $payload), $message);
        }
    }

    /**
     * @Given /^the "([^"]*)" property is a boolean$/
     */
    public function thePropertyIsABoolean($property)
    {
        $payload = $this->getScopePayload();

        PhpUnit::assertTrue(
            gettype($this->arrayGet($payload, $property)) == 'boolean',
            "Asserting the [$property] property in current scope [{$this->scope}] is a boolean."
        );
    }

    /**
     * @Given /^the "([^"]*)" property is a boolean equalling "([^"]*)"$/
     */
    public function thePropertyIsABooleanEqualling($property, $expectedValue)
    {
        $payload = $this->getScopePayload();
        $actualValue = $this->arrayGet($payload, $property);

        if (!in_array($expectedValue, ['true', 'false'])) {
            throw new \InvalidArgumentException("Testing for booleans must be represented by [true] or [false].");
        }

        $this->thePropertyIsABoolean($property);

        PhpUnit::assertSame(
            $actualValue,
            $expectedValue == 'true',
            "Asserting the [$property] property in current scope [{$this->scope}] is a boolean equalling [$expectedValue]."
        );
    }

    /**
     * @Given /^the "([^"]*)" property is a float$/
     */
    public function thePropertyIsAFloat($property)
    {
        $payload = $this->getScopePayload();

        PhpUnit::assertTrue(
            is_float($this->arrayGet($payload, $property)),
            "Asserting the [$property] property in current scope [{$this->scope}] is a float: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is a string$/
     */
    public function thePropertyIsAString($property)
    {
        $payload = $this->getScopePayload();
        PhpUnit::assertTrue(
            is_string($this->arrayGet($payload, $property)),
            "Asserting the [$property] property in current scope [{$this->scope}] is a string: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is a string equalling "([^"]*)"$/
     */
    public function thePropertyIsAStringEqualling($property, $expectedValue)
    {
        $payload = $this->getScopePayload();

        $this->thePropertyIsAString($property);

        $actualValue = $this->arrayGet($payload, $property);

        PhpUnit::assertSame(
            $actualValue,
            $expectedValue,
            "Asserting the [$property] property in current scope [{$this->scope}] is a string equalling [$expectedValue]."
        );
    }

    /**
     * @Given /^the "([^"]*)" property is a string or null$/
     */
    public function thePropertyIsAStringOrNull($property)
    {
        $typeTest = function ($propertyValue) {
            return (is_string($propertyValue) OR ($propertyValue === null));
        };
        $message = function ($property, $payload) {
            return "Asserting the [$property] property in current scope [{$this->scope}] is a string or null: ".json_encode($payload);
        };

        $this->assertNullOrType($typeTest, $property, $message);
    }

    /**
     * @Given /^the "([^"]*)" property is an array$/
     */
    public function thePropertyIsAnArray($property)
    {
        $payload = $this->getScopePayload();

        $actualValue = $this->arrayGet($payload, $property);

        PhpUnit::assertTrue(
            is_array($actualValue),
            "Asserting the [$property] property in current scope [{$this->scope}] is an array: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is an empty array$/
     */
    public function thePropertyIsAnEmptyArray($property)
    {
        $payload = $this->getScopePayload();
        $scopePayload = $this->arrayGet($payload, $property);

        PhpUnit::assertTrue(
            is_array($scopePayload) and $scopePayload === [],
            "Asserting the [$property] property in current scope [{$this->scope}] is an empty array: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is an integer$/
     */
    public function thePropertyIsAnInteger($property)
    {
        $payload = $this->getScopePayload();

        PhpUnit::assertTrue(
            is_int($this->arrayGet($payload, $property)),
            "Asserting the [$property] property in current scope [{$this->scope}] is an integer: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is an integer equalling "([^"]*)"$/
     */
    public function thePropertyIsAnIntegerEqualling($property, $expectedValue)
    {
        $payload = $this->getScopePayload();
        $actualValue = $this->arrayGet($payload, $property);

        $this->thePropertyIsAnInteger($property);

        PhpUnit::assertSame(
            $actualValue,
            (int)$expectedValue,
            "Asserting the [$property] property in current scope [{$this->scope}] is an integer equalling [$expectedValue]."
        );
    }

    /**
     * @Given /^the "([^"]*)" property is an integer or null$/
     */
    public function thePropertyIsAnIntegerOrNull($property)
    {
        $typeTest = function ($propertyValue) {
            return is_int($propertyValue) OR ($propertyValue === null);
        };

        $message = function ($property, $payload) {
            return "Asserting the [$property] property in current scope [{$this->scope}] is an integer or null: ".json_encode($payload);
        };

        $this->assertNullOrType($typeTest, $property, $message);
    }

    /**
     * @Given /^the "([^"]*)" property is an object$/
     */
    public function thePropertyIsAnObject($property)
    {
        $payload = $this->getScopePayload();

        $actualValue = $this->arrayGet($payload, $property);

        PhpUnit::assertTrue(
            is_object($actualValue),
            "Asserting the [$property] property in current scope [{$this->scope}] is an object: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the "([^"]*)" property is either:$/
     */
    public function thePropertyIsEither($property, PyStringNode $options)
    {
        $payload = $this->getScopePayload();
        $actualValue = $this->arrayGet($payload, $property);

        $valid = explode("\n", (string)$options);

        PhpUnit::assertTrue(
            in_array($actualValue, $valid),
            sprintf(
                "Asserting the [%s] property in current scope [{$this->scope}] is in array of valid options [%s].",
                $property,
                implode(', ', $valid)
            )
        );
    }

    /**
     * @Given /^the "([^"]*)" property is null$/
     */
    public function thePropertyIsNull($property)
    {
        $payload = $this->getScopePayload();

        PhpUnit::assertTrue(
            is_null($this->arrayGet($payload, $property)),
            "Asserting the [$property] property in current scope [{$this->scope}] is an null: ".json_encode($payload)
        );
    }

    /**
     * @Given /^the response is json$/
     */
    public function theResponseIsJson()
    {
        PhpUnit::assertSame(strtolower($this->response->getHeader('Content-Type')[0]), 'application/vnd.wps_api.v4+json');
        PhpUnit::isJson($this->response->getBody());
    }

    /**
     * @param $scope
     */
    protected function addToCurrentScope($scope)
    {
        $this->scope = $this->scope.'.'.$scope;
    }

    /**
     * Get an item from an array using "dot" notation.
     *
     * @copyright   Taylor Otwell
     * @link        http://laravel.com/docs/helpers
     *
     * @param       array  $array
     * @param       string $key
     * @param       mixed  $default
     *
     * @return      mixed
     */
    protected function arrayGet($array, $key)
    {
        if (is_null($key)) {
            return $array;
        }

        // if (isset($array[$key])) {
        //     return $array[$key];
        // }

        foreach (explode('.', $key) as $segment) {

            if (is_object($array)) {
                if (!property_exists($array, $segment)) {
                    throw new OutOfBoundsException($segment.' property does not exist');
                }
                if (!isset($array->{$segment})) {
                    return false;
                }
                $array = $array->{$segment};

            } elseif (is_array($array)) {
                if (!array_key_exists($segment, $array)) {
                    throw new OutOfRangeException($key.' key does not exist');
                }
                $array = $array[$segment];
            }
        }

        return $array;
    }

    protected function assertNullOrType(\Closure $typeTest, $property, \Closure $message)
    {
        $payload = $this->getScopePayload();
        $propertyValue = $this->arrayGet($payload, $property);
        $result = $typeTest($propertyValue);

        PhpUnit::assertTrue($result, $message($property, $payload));
    }

    /**
     * @param array $needle
     * @param array $haystack
     */
    protected function compareArrays(array $needle, array $haystack)
    {
        foreach ($needle as $key => $value) {
            PhpUnit::assertArrayHasKey($key, $haystack);
            PhpUnit::assertTrue(in_array($value, $haystack));
        }
    }

    /**
     * @return string
     */
    protected function compileUri()
    {
        if ($this->query) {
            return $this->resource.'?'.$this->query;
        }

        return $this->resource;
    }

    /**
     * Return the response payload from the current response.
     *
     * @return  mixed
     */
    protected function decodePayload($payload)
    {
        $json = json_decode($payload, true);

        if (json_last_error() !== JSON_ERROR_NONE) {
            $message = 'Failed to decode JSON body ';

            switch (json_last_error()) {
                case JSON_ERROR_DEPTH:
                    $message .= '(Maximum stack depth exceeded).';
                    break;
                case JSON_ERROR_STATE_MISMATCH:
                    $message .= '(Underflow or the modes mismatch).';
                    break;
                case JSON_ERROR_CTRL_CHAR:
                    $message .= '(Unexpected control character found).';
                    break;
                case JSON_ERROR_SYNTAX:
                    $message .= '(Syntax error, malformed JSON).';
                    break;
                case JSON_ERROR_UTF8:
                    $message .= '(Malformed UTF-8 characters, possibly incorrectly encoded).';
                    break;
                default:
                    $message .= '(Unknown error).';
                    break;
            }

            throw new Exception($message);
        }

        return $json;
    }

    /**
     * @param $requestPayload
     *
     * @return array
     */
    protected function getIdsFromNestedArray(array $requestPayload)
    {
        $ids = array_map(function ($array) {

            if (isset($array['id'])) {
                return $array['id'];
            }
        }, $requestPayload);

        return $ids;
    }

    protected function getRandomDataForKey($key)
    {
        if (!array_key_exists($key, $this->data)) {
            return null;
        }

        return $this->data[$key];
    }

    /**
     * Checks the response exists and returns it.
     *
     * @return \Psr\Http\Message\ResponseInterface
     * @throws \Exception
     */
    protected function getResponse()
    {
        if (!$this->response) {
            throw new \Exception("You must first make a request to check a response.");
        }

        return $this->response;
    }

    /**
     * Returns the payload from the current scope within
     * the response.
     *
     * @param bool $forceUpdate
     *
     * @return mixed
     * @throws Exception
     */
    protected function getScopePayload($forceUpdate = false)
    {
        if (!$this->responsePayload OR $forceUpdate === true) {
            $this->responsePayload = $this->decodePayload($this->getResponse()->getBody());
        }

        if (!$this->scope) {
            return $this->responsePayload;
        }

        return $this->arrayGet($this->responsePayload, $this->scope);
    }

    /**
     * @param $options
     */
    protected function guzzleRequestConfig(array $options)
    {
        return array_merge($options, $this->guzzleConfig);
    }

    protected function makeRequest()
    {
        $method = strtolower($this->httpMethod);

        $uri = $this->compileUri();

        try {
            switch ($this->httpMethod) {
                case 'PATCH':
                case 'PUT':
                case 'POST':
                    $this->response = $this
                        ->client
                        ->$method($uri, $this->guzzleRequestConfig(['headers' => $this->headers, 'body' => $this->requestPayload]));
                    break;

                default:
                    $this->response = $this
                        ->client
                        ->$method($uri, $this->guzzleRequestConfig(['headers' => $this->headers]));
            }
        } catch (BadResponseException $e) {

            $response = $e->getResponse();

            // Sometimes the request will fail, at which point we have
            // no response at all. Let Guzzle give an error here, it's
            // pretty self-explanatory.
            if ($response === null) {
                throw $e;
            }

            $this->response = $e->getResponse();
        }
    }

    /**
     * @param $scope
     */
    protected function maybeSetSubScope($scope)
    {
        if ($this->scope) {
            $this->addToCurrentScope($scope);
        } else {
            $this->scope = $scope;
        }
    }

    protected function putRandomDataInQuery()
    {
        $this->query = $this->testingHelper->putDataInQueryString($this->query, $this->data);
    }

    /**
     * @param null|array $ids This overrides the ids property
     * @param string     $idPlaceholder
     */
    protected function putRandomIdsInResource($ids = null, $idPlaceholder = '{id}')
    {
        if (is_null($this->ids)) {
            $this->ids = $this->testingHelper->getRandomIdsForUri($this->resource, function ($uri, $placeholder) {
                /** @var \LaraPackage\Api\Config\ApiVersion $config */
                $config = \App::make(\LaraPackage\Api\Contracts\Config\ApiVersion::class);

                return $config->resourceIdMap($uri, $this->apiVersion);
            });
            $this->resource = $this->testingHelper->putIdsInUri($this->resource, $this->ids);
        } else {
            // this adds the ids to the resource: /sites/{id} to /sites/1
            $ids = (null === $ids) ? $this->ids : $ids;
            $this->resource = $this->testingHelper->putIdsInUri($this->resource, $ids, $idPlaceholder);
        }
    }

    /**
     * @param $httpMethod
     * @param $uri
     */
    protected function setResourceAndQuery($httpMethod, $uri)
    {
        $this->httpMethod = $httpMethod;
        $this->resource = $this->resourceParser->getResource($uri);
        $this->query = $this->resourceParser->getQuery($uri);
    }
}
